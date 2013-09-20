(use bitstring)

(define-record midifile format time-division tracks)

(define (midi-file-open name)
  (bitmatch (read-all name)
    ((("MThd") (#x00000006 32) (rest bitstring))
     (parse-midi-format rest))
    (else
     (error "invalid midi file signature"))))

(define (parse-midi-format data)
  (bitmatch data
    (((format 16) (num-tracks 16) (time-division 16 bitstring) (tracks bitstring))
     (if (> format 2)
        (error "unsupported midi format" format))
     (unless (and (= format 0) (= num-tracks))
       (error "this midi format support only one track"))
     (unless (and (= format 1) (> num-tracks 1))
       (error "this midid format should have thow or more tracks"))
     (parse-midi-tracks num-tracks tracks (parse-midi-time-format timi-division)))
    (else
     (error "invalid midi file header"))))

(define (parse-midi-time-format time-division)
  (bitmatch 
    (((0 1) (ticks 15))
     (list 'ticks-per-beat ticks))
    (((1 1) (frames 7) (clocks 8))
     (list 'frames-per-second frames clocks))))

(define (parse-midi-tracks num-tracks tracks time-format)
  (let loop ((i 1)
             (data tracks)
             (acc (list)))
    (bitmatch data
      ((("MTrk") (track-size 32) (track-data (* 8 track-size)) (rest bitstring))
       (if (< i num-tracks)
           (loop (add1 i)
                 (cons (parse-midi-track-events track-data) acc)
                 rest)
           (reverse acc)))
      (else
       (error "invalid track header" i)))))

(define (accumulate-length len d)
  (bitwise-ior (arithmetic-shift len 7) d))

(define (parse-variable-length stream len)
  (bitmatch stream
    (((0 1) (d 7) (rest bitstring))
     (valules (accumulate-length len d) rest))
    (((1 1) (d 7) (rest bitstring))
     (parse-variable-length rest (accumulate-length len d)))))

(define (parse-midi-track-events track-data)
  (let loop ((acc (list))
             (track track-data))
    (receive (delta rest)
             (parse-variable-length track 0)
      (bitmatch rest
        (()
         (reverse acc))
        (((#x8 4) (channel 4) (note 8) (velocity 8) (rest bitstring))
         (loop (cons (list delta 'note-off channel note velocity) acc) rest))
        (((#x9 4) (channel 4) (note 8) (velocity 8) (rest bitstring))
         (loop (cons (list delta 'note-on channel note velocity) acc) rest))
        (((#xA 4) (channel 4) (note 8) (value 8) (rest bitstring))
         (loop (cons (list delta 'note-aftertouch channel note value)) rest))
        (((#xB 4) (channel 4) (number 8) (value 8) (rest bitstring))
         (loop (cons (list delta 'controller channel number value)) rest))
        (((#xC 4) (channel 4) (number 8) (unused 8) (rest bitstring))
         (loop (cons (list delta 'program-change channel number)) rest))
        (((#xD 4) (channel 4) (value 8) (unused 8) (rest bitstring))
         (loop (cons (list delta 'channel-aftertouch channel value)) rest))
        (((#xE 4) (channel 4) (value 16) (rest bitstring))
         (loop (cons (list delta 'pitch-bend channel value)) rest))
        (((#xFF) (type 8) (event-data bitstring))
         (receive (meta-event rest)
                  (parse-midi-meta-event delta type event-data)
           (loop (cons meta-event acc) rest)))
        (((#xF7) (event-data bitstring))
         (receive (sysex-event rest)
                  (parse-authorization-sysex-event delta event-data)
           (loop (cons sysex-event acc) rest)))
        (else
         (parse-midi-sysex-event delta rest))))))

(define (parse-midi-meta-event delta type event-data)
  (receive (var-data len)
           (parse-variable-length event-data 0)
    (bitmatch var-data
      (((data (* len 8) bitstring) (rest bitstring))
       (values (list delta 'meta-event type data) rest))
      (else
       (error "invalid meta-event length")))))

(define (parse-authorization-sysex-event delta event-data)
  (receive (len var-data)
           (parse-variable-length event-data 0)
    (bitmatch var-data
      (((data (* len 8) bitstring) (rest bitstring))
       (values (list delta 'sysex-event data) rest))
      (else
       (error "invalid authorization sysex event")))))

(define (parse-midi-sysex-event delta event-data)
  (let loop ((acc (bitstring-create))
             (event-data event-data))
    (bitmatch event-data
      (((#xF0) (message bitstring))
       (receive (len var-data)
                (parse-variable-length message 0)
         (bitmatch var-data
           (((data (* len 8) bitstring) (#xF7) (rest bitstring))
            ; normal message
            (values (list delta 'sysex-event (bitstring-append acc data)) rest))
           (((data (* len 8) bitstring) (rest bitstring))
            ; divided message
            (loop (bitstring-append acc data) rest)))))
      (else
       (error "invalid midi event")))))

