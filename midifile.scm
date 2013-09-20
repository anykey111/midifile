(module midifile
  (midifile-load midifile-save)
  (import scheme chicken)
  (require-extension bitstring matchable utils posix)

(define-record midifile format division tracks)

(define (midifile-load name)
  (bitmatch (read-all name)
    ((("MThd") (#x00000006 32) (rest bitstring))
     (parse-midi-format rest))
    ((rest bitstring)
     (error "invalid midi file signature" (failure-location rest)))))

(define (midifile-save mf name)
  (let ((fd (file-open name (+ open/write open/creat open/trunc))))
    (file-write fd
       (bitstring->blob
        (bitconstruct
         ("MThd")
         (#x00000006 32)
         ((midifile-format mf) 16)
         ((length (midifile-tracks mf)) 16)
         ((store-midi-time-division mf) bitstring)
         ((store-midi-tracks mf) bitstring))))
    (file-close fd)))

(define (store-midi-time-division mf)
  (match (midifile-division mf)
    (('ticks-per-beat ticks)
     (bitconstruct (0 1) (ticks 15)))
    (('frames-per-second frames clocks)
     (bitconstruct (1 1) (frames 7) (clocks 8)))))

(define (store-midi-tracks mf)
  (let loop ((acc (list))
             (tracks (midifile-tracks mf)))
    (match tracks
      (()
       (let* ((track-data (apply bitstring-append (reverse acc)))
              (track-size (quotient (bitstring-length track-data) 8)))
         (bitconstruct ("MTrk")
                       (track-size 32)
                       (track-data bitstring))))
      ((track . rest)
       (loop (cons (store-midi-track track) acc) rest)))))

(define (make-status-byte status channel)
  (bitwise-ior (arithmetic-shift status 4) channel))
           
(define (store-midi-track track)
  (define acc (bitstring-create))
  (let loop ((running-status 0)
             (lst track))
    (match lst
     (()
       acc)
     (((delta 'sysex-event status data) . rest)
       ; todo: handle divided message
       (bitstring-append! acc (bitconstruct ((store-variable-length delta) bitstring)
                                            (status 8)
                                             ((store-variable-length (blob-size data)) bitstring)
                                            (data bitstring)))
       (loop status rest))
     (((delta 'meta-event type data) . rest)
       (bitstring-append! acc (bitconstruct ((store-variable-length delta) bitstring)
                                            (#xFF)
                                            (type 8)
                                            ((store-variable-length (blob-size data)) bitstring)
                                            (data bitstring)))
       (loop #xFF rest))
     (((delta midi-event status channel . args) . rest)
      (let ((status-byte (make-status-byte status channel)))
        (if (= running-status status-byte)
          (begin ; omit status byte
            (bitstring-append! acc (bitconstruct
                                    ((store-variable-length delta) bitstring)
                                    ((store-midi-event-data midi-event args) bitstring)))
            (loop running-status rest))
          (begin ; setup new running status
            (bitstring-append! acc (bitconstruct
                                    ((store-variable-length delta) bitstring)
                                    (status-byte 8)
                                    ((store-midi-event-data midi-event args) bitstring)))
            (loop status-byte rest)))))
      ((midi-event . rest)
       (error "unknown midi event" midi-event)))))
       
      
(define (store-midi-event-data id args)
  (match (cons id args)
    (('note-on note velocity)
     (bitconstruct (note 8) (velocity 8)))
    (('note-off note velocity)
     (bitconstruct (note 8) (velocity 8)))
    (('note-aftertouch note value)
     (bitconstruct (note 8) (value 8)))
    (('controller number value)
     (bitconstruct (number 8) (value 8)))
    (('program-change number)
     (bitconstruct (number 8) (0 8)))
    (('channel-aftertouch value)
     (bitconstruct (value 8) (0 8)))
    (('pitch-bend value)
     (bitconstruct (value 16)))))         

(define (store-variable-length len #!optional (acc (list)))
  (if (and (zero? len) (not (eq? '() acc)))
    (apply bitstring-append (reverse acc))
    (store-variable-length (arithmetic-shift len -7)
                           (cons (bitconstruct (1 1) ((bitwise-and len #x7F) 7))
                                 acc))))

(define (parse-midi-format data)
  (bitmatch data
    (((format 16) (num-tracks 16) (time-division 16 bitstring) (tracks bitstring))
     (make-midifile format
                    (parse-midi-time-format time-division)
                    (parse-midi-tracks num-tracks tracks)))
    (else
     (error "invalid midi file header" (failure-location data)))))

(define (parse-midi-time-format time-division)
  (bitmatch time-division
    (((0 1) (ticks 15))
     (list 'ticks-per-beat ticks))
    (((1 1) (frames 7) (clocks 8))
     (list 'frames-per-second frames clocks))))

(define (parse-midi-tracks num-tracks tracks)
  (let loop ((i 1)
             (acc (list))
             (track tracks))
    (bitmatch track
      ((("MTrk") (track-size 32) (track-data (* 8 track-size) bitstring) (rest bitstring))
       (if (< i num-tracks)
           (loop (add1 i)
                 (cons (parse-midi-track-events track-data) acc)
                 rest)
           (reverse acc)))
      (else
       (error "invalid track header" i (failure-location track))))))

(define (accumulate-length len d)
  (bitwise-ior (arithmetic-shift len 7) d))

(define (parse-variable-length data len)
  (bitmatch data
    (((0 1) (d 7) (rest bitstring))
     (values (accumulate-length len d) rest))
    (((1 1) (d 7) (rest bitstring))
     (parse-variable-length rest (accumulate-length len d)))
    (else
     (error "missing variable length offset:" (failure-location data)))))

(define (parse-midi-track-events track-data)
  (let loop ((acc (list))
             (status 0)
             (channel 0)
             (track track-data))
    (if (zero? (bitstring-length track))
      (reverse acc)
      (receive (event status channel rest)
               (parse-midi-event track status channel)
        (loop (cons event acc) status channel rest)))))

(define (parse-midi-event track current-status current-channel)
  (receive (delta var-data)
           (parse-variable-length track 0)
    (receive (status channel rest)
             (parse-midi-status-byte var-data current-status current-channel)
      (bitmatch rest
        (((check (= status #x8)) (note 8) (velocity 8) (rest bitstring))
         (values (list delta 'note-off status channel note velocity) status channel rest))
        (((check (= status #x9)) (note 8) (velocity 8) (rest bitstring))
          (values (list delta 'note-on status channel note velocity) status channel rest))
        (((check (= status #xA)) (note 8) (value 8) (rest bitstring))
         (values (list delta 'note-aftertouch status channel note value) status channel rest))
        (((check (= status #xB)) (number 8) (value 8) (rest bitstring))
         (values (list delta 'controller status channel number value) status channel rest))
        (((check (= status #xC)) (number 8) (unused 8) (rest bitstring))
         (values (list delta 'program-change status channel number) status channel rest))
        (((check (= status #xD)) (value 8) (unused 8) (rest bitstring))
         (values (list delta 'channel-aftertouch status channel value) status channel rest))
        (((check (= status #xE)) (value 16) (rest bitstring))
         (values (list delta 'pitch-bend status channel value) status channel rest))
        (((check (= status #xFF)) (type 8) (event-data bitstring))
         (receive (meta-event rest)
                  (parse-midi-meta-event delta type event-data)
                  (values meta-event status channel rest)))
        (((check (= status #xF7)) (event-data bitstring))
         (receive (sysex-event rest)
                  (parse-authorization-sysex-event delta event-data)
                  (values sysex-event status channel rest)))
        (((check (= status #xF0)) (event-data bitstring))
         (receive (sysex-event rest)
                  (parse-midi-sysex-event delta event-data)
                  (values sysex-event status channel rest)))
        (else
         (error "unknown midi event" (failure-location rest)))))))

(define (parse-midi-status-byte event-data current-status current-channel)
  (bitmatch event-data
    (((0 1) (rest bitstring))
     ; reuse status byte
     (values current-status current-channel event-data))
    (((s 8) (check (or (= s #xFF) (= s #xF0) (= s #xF7))) (rest bitstring))
     ; setup meta event or sysex event status
     (values s 0 rest))
    (((status 4) (channel 4) (rest bitstring))
     ; change channel command status
     (values status channel rest))))

(define (parse-midi-meta-event delta type event-data)
  (receive (len var-data)
           (parse-variable-length event-data 0)
    (bitmatch var-data
      (((data (* len 8) bitstring) (rest bitstring))
       (values (list delta 'meta-event type (bitstring->blob data)) rest))
      (else
       (error "invalid meta-event length" (failure-location var-data))))))

(define (parse-authorization-sysex-event delta event-data)
  (receive (len var-data)
           (parse-variable-length event-data 0)
    (bitmatch var-data
      (((data (* len 8) bitstring) (rest bitstring))
       (values (list delta 'sysex-event #xF7 (bitstring->blob data)) rest))
      (else
       (error "invalid authorization sysex event" (failure-location var-data))))))

(define (parse-midi-sysex-event delta event-data)
  (define (concat-sysex-data lst)
    (bitstring->blob (apply bitstring-append (reverse lst))))
  (let loop ((acc (bitstring-create))
             (event-data event-data))
    (receive (len var-data)
             (parse-variable-length event-data 0)
      (bitmatch var-data
        (((data (* len 8) bitstring) (#xF7) (rest bitstring))
         ; normal message
         (values (list delta 'sysex-event #xF0 (concat-sysex-data (cons data acc))) rest))
        (((data (* len 8) bitstring) (#xF0) (rest bitstring))
         ; divided message
         (loop (cons data acc) rest))
        (else
         (error "invalid sysex-event" (failure-location var-data)))))))

(define (failure-location rest)
  (let ((offset (/ (bitstring-offset rest) 8)))
    (list 'offset offset
          'bytes (bitstring->blob
                   (bitmatch rest
                     (((first-bytes 64 bitstring) (_rest bitstring))
                      first-bytes)
                     (else rest))))))

)
