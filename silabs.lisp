(in-package #:joff)

(defconstant +silabs-i2c-address+ #x54)

;; ordered alist of name . address
(defconstant +ts7250v3-anspec+
  #((:5v . 0) (:vdd-soc . 2) (:3.3v . 4) (:10-to-48v . 6) (:vdd-arm . 10) (:temp . 20)))

(defun silab-outw (subadr u16)
  (declare (type (unsigned-byte 16) subadr u16))
  (let ((v (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (nibbles:ub16ref/be v 0) u16)
    (linux-i2c-eeprom-write +silabs-i2c-address+ subadr v)))

(defun silab-outb (subadr u8)
  (declare (type (unsigned-byte 16) subadr)
           (type (unsigned-byte 8) u8))
  (let ((v (make-array 1 :element-type '(unsigned-byte 8) :initial-element u8)))
    (linux-i2c-eeprom-write +silabs-i2c-address+ subadr v)))

(defun silab-inb (subadr)
  (declare (type (unsigned-byte 16) subadr))
  (aref (linux-i2c-eeprom-read +silabs-i2c-address+ subadr 1) 0))

(defun silab-inw (subadr)
  (declare (type (unsigned-byte 16) subadr))
  (nibbles:ub16ref/be (linux-i2c-eeprom-read +silabs-i2c-address+ subadr 2) 0))

(defun silab-write (subadr buf)
  (declare (type (unsigned-byte 16) subadr))
  (linux-i2c-eeprom-write +silabs-i2c-address+ subadr
                          (coerce buf '(vector (unsigned-byte 8)))))

(defun silab-read (subadr len)
  (declare (type (unsigned-byte 16) subadr) (type fixnum len))
  (linux-i2c-eeprom-read +silabs-i2c-address+ subadr len))

(defun silab-build-string ()
  (let ((x (silab-read 4096 80)))
    (map 'string #'code-char (subseq x 0 (position 0 x)))))

(defun silab-an (anspec)
  "Retreive 16-bit silabs analog values."
  (declare (type simple-vector anspec))
  (let ((buf (silab-read 0 (+ 2 (cdr (aref anspec (1- (length anspec))))))))
    (map 'list (lambda (x) (cons (car x) (nibbles:ub16ref/be buf (cdr x)))) anspec)))

(defun silab-watchdog-ms ()
    (* 10 (nibbles:ub32ref/le (silab-read 1024 4) 0)))

(defun silab-mac (&optional mac-to-write)
  "If arg is given, it is a new MAC address to write and must not be more than 6 bytes."
  (if mac-to-write
      (silab-write 28 mac-to-write)
      (silab-read 28 6)))

(defun an-accumulate (hist-or-nil an-sample-alist)
  "Accumulates new numeric readings from an-sample-alist into history hash hist."
  (declare (type (or null hash-table) hist-or-nil) (type list an-sample-alist))
  (let ((hist (if hist-or-nil hist-or-nil (make-hash-table))))
    (mapc
     (lambda (x)
       (let ((hash (gethash (car x) hist))
             (val (cdr x)) tm)
         (declare (type fixnum val) (type hash-table hash) (optimize speed))
         (unless hash
           (unless tm (setf tm (local-time:now)))
           (setf hash (make-hash-table)
                 (gethash (car x) hist) hash
                 (gethash :sum hash) 0
                 (gethash :start-timestamp hash) tm
                 (gethash :first-sample hash) val
                 (gethash :nsamples hash) 0))
         (if (gethash (cdr x) hash)
             (incf (gethash val hash))
             (setf (gethash val hash) 1))
         (setf (gethash :last-sample hash) val)
         (incf (gethash :sum hash) val)
         (incf (gethash :nsamples hash))))
     an-sample-alist)
    hist))

(defun an-summarize (hist &key and-reset!)
  "Returns calculation alists for nsamples, median, mean, min, max, histogram, and stdev."
  (labels
      ((summarize-chan (hash)
         (let ((sum (gethash :sum hash))
               (last (gethash :last-sample hash))
               (nsamp (gethash :nsamples hash))
               (first (gethash :first-sample hash))
               (start (gethash :start-timestamp hash))
               (vec (make-array (hash-table-count hash) :fill-pointer 0))
               min max v mean ret)
           (when (or (not nsamp) (not sum) (zerop nsamp)) (return-from summarize-chan nil))
           (maphash (lambda (key val)
                      (when (numberp key)
                        (unless min (setf min key max key v 0 mean (/ sum nsamp)))
                        (incf v (* val (expt (- key mean) 2)))
                        (vector-push (vector key val) vec)
                        (when (> key max) (setf max key))
                        (when (< key min) (setf min key))))
                    hash)
           (setf vec (sort vec (lambda (x y) (< (aref x 0) (aref y 0)))))
           (push (cons :histogram vec) ret)
           (push (cons :stdev (/ v nsamp)) ret)
           (push (cons :nsamples nsamp) ret)
           (push (cons :first-sample first) ret)
           (push (cons :start-timestamp (local-time:format-timestring nil start)) ret)
           (push (cons :min min) ret)
           (push (cons :max max) ret)
           (push (cons :last-sample last) ret)
           (push (cons :median (quantile (floor nsamp 2) vec)) ret)
           (push (cons :mean mean) ret)
           ret))
       (quantile (q sorted-histogram-vec)
         "q is in samples.  For the median it would be nsamp/2."
         (let ((n 0))
           (aref (find-if (lambda (x) (incf n (aref x 1)) (>= n q)) sorted-histogram-vec) 0)))
       ;; (quartiles (q sorted-histogram-vec)
       ;;   (let* ((n 0) (lo (floor q 4)) (mid (floor q 2)) (hi (floor (* 3 q) 4))
       ;;          (l (find-if (lambda (x) (incf n (aref x 1)) (>= n lo)) sorted-histogram-vec))
       ;;          (m ))))
       )
    (let (ret)
      (maphash (lambda (key val) (push (cons key (summarize-chan val)) ret)) hist)
      (when and-reset! (setf hist nil))
      (push (cons :timestamp (local-time:format-timestring nil (local-time:now))) ret))))

(defun an-test (hash)
  (loop
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (an-accumulate hash (silab-an +ts7250v3-anspec+))
    (sleep 1/5)
    (format t "~a~%" (cl-json:encode-json-to-string (an-summarize hash)))))



(defun hex-bytes-string (bytes)
  "Returns a XX:XX:XX:XX:XX string where XX are hex bytes."
  (let ((l (coerce bytes 'list)))
    (format nil "~2,'0x~{:~2,'0x~}" (car l) (cdr l))))

(defun silab-wdog-set (millis &key (mode 1))
  (multiple-value-bind (q r) (floor millis 10)
    (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
      (setf (nibbles:ub32ref/le buf 0) (if (zerop r) q (1+ q)))
      (setf (aref buf 4) mode)
      (silab-write 1024 buf))))

(defun silab-wdog-feed () (silab-outb 1028 1))

(defun silab-sleep (millis) (silab-wdog-set millis :mode 2))

(defun silab-flags-set (n)
  (declare (type (integer 0 7) n))
  (let* ((old (silab-inb 23))
         (new (dpb 1 (byte 1 n) old)))
    (when (/= old new) (silab-outb 23 new))))

(defun silab-flags-clr (n)
  (declare (type (integer 0 7) n))
  (let* ((old (silab-inb 23))
         (new (dpb 0 (byte 1 n) old)))
    (when (/= old new) (silab-outb 23 new))))

(defun silab-status-ts7250v3 ()
  (let ((x (silab-an +ts7250v3-anspec+))
        (reg22 (silab-inb 22)))
    (push (cons :watchdog-ms (silab-watchdog-ms)) x)
    (push (cons :uc-build (silab-build-string)) x)
    (push (cons :uc-version (silab-inb 2048)) x)
    ;; (push (cons :mac (hex-bytes-string (silab-mac))) x)
    (push (cons :watchdog-enabled (ldb (byte 1 6) reg22)) x)
    (push (cons :last-reboot-was-from-watchdog (ldb (byte 1 7) (silab-inb 1028))) x)
    (push (cons :usb-connected (ldb (byte 1 4) reg22)) x)))

(defun silab-cmd (cmd)
  (labels
      ((hex (s) (parse-integer s :radix 16 :junk-allowed t))
       (dec (s) (parse-integer s :radix 10 :junk-allowed t))
       (mac (s) (map '(vector (unsigned-byte 8)) #'hex
                     (split-sequence:split-sequence #\: s)))
       (%peekstream (o n) (binascii:encode (silab-read (hex o) (hex n)) :hex))
       (%pokestream (o d) (silab-write (hex o) (binascii:decode d :hex)))
       (f (argv)
         (match argv
           ((list "status") (silab-status-ts7250v3))
           ((list "an") (silab-an +ts7250v3-anspec+))
           ((list "wdog") (ldb (byte 1 6) (silab-inb 22)))
           ((list "wdog" "feed") (silab-wdog-feed))
           ((list "wdog" "set" ms) (silab-wdog-set (dec ms)))
           ((list "wdog" "disable") (silab-wdog-set 0))
           ((list "wdog" "expired") (ldb (byte 1 7) (silab-inb 1028)))
           ((list "fan" "enable") (silab-outb 1032 0))
           ((list "fan" "disable") (silab-outb 1032 1))
           ((list "usb") (ldb (byte 1 4) (silab-inb 22)))
           ((list "flags" n) (ldb (byte 1 (dec n)) (silab-inb 23)))
           ((list "flags" "set" n) (silab-flags-set (dec n)))
           ((list "flags" "clr" n) (silab-flags-clr (dec n)))
           ((list "sleep" ms) (silab-sleep (dec ms)))
           ((list "build") (silab-build-string))
           ((list "mac") (hex-bytes-string (silab-mac)))
           ((list* "pokestream" o (cons d rest)) (%pokestream o d) (f rest))
           ((list* "peekstream" o (cons n rest)) (cons (%peekstream o n) (f rest)))
           ((list* "peek8" (cons o rest)) (cons (silab-inb (hex o)) (f rest)))
           ((list* "peek16" (cons o rest)) (cons (silab-inw (hex o)) (f rest)))
           ((list* "poke8" o (cons v rest)) (silab-outb (hex o) (hex v)) (f rest))
           ((list* "poke16" o (cons v rest)) (silab-outw (hex o) (hex v)) (f rest)))))
    (let ((ret (f (split-sequence:split-sequence #\Space cmd))))
      (match ret
        ((cons x nil) x)
        (otherwise ret)))))

