(in-package #:joff)

(defun linux-i2c-write-then-read (i2c-adr write-data read-len &key (i2c-dev "/dev/i2c-0"))
  (declare (type (unsigned-byte 16) i2c-adr read-len) (optimize safety))
  (let ((ret-buf (cffi:make-shareable-byte-vector read-len))
        (nw (length write-data))
        (wvec (coerce write-data '(vector (unsigned-byte 8))))
        (fd (if (integerp i2c-dev) i2c-dev
                (osicat-posix:open i2c-dev osicat-posix:o-rdwr))))
    (cffi:with-pointer-to-vector-data (bufptr ret-buf)
      (cffi:with-foreign-objects ((packets '(:struct i2c-rdwr-ioctl-data))
                                  (i2c-msgs '(:struct i2c-msg) 2)
                                  (wbuf :uint8 nw))
        (cffi:lisp-array-to-foreign wvec wbuf (list :array :uint8 nw))
        (cffi:with-foreign-slots ((addr flags len buf)
                                  (cffi:mem-aptr i2c-msgs '(:struct i2c-msg) 0) (:struct i2c-msg))
          (setf addr i2c-adr flags 0 len 2 buf (cffi:mem-aptr wbuf :uint8)))
        (cffi:with-foreign-slots ((addr flags len buf)
                                  (cffi:mem-aptr i2c-msgs '(:struct i2c-msg) 1) (:struct i2c-msg))
          (setf addr i2c-adr flags :i2c-m-rd len read-len buf bufptr))
        (cffi:with-foreign-slots ((msgs nmsgs) packets (:struct i2c-rdwr-ioctl-data))
          (setf msgs i2c-msgs nmsgs 2))
        (osicat-posix:ioctl fd +i2c-rdwr+ packets)
        (unless (integerp i2c-dev) (osicat-posix:close fd))
        ret-buf))))

(defun linux-i2c-write (i2c-adr data &key (i2c-dev "/dev/i2c-0"))
  (declare (type (unsigned-byte 16) i2c-adr) (optimize safety))
  (let ((datalen (length data))
        (wvec (coerce data '(vector (unsigned-byte 8))))
        (fd (if (integerp i2c-dev) i2c-dev
                (osicat-posix:open i2c-dev osicat-posix:o-rdwr))))
    (cffi:with-foreign-objects ((packets '(:struct i2c-rdwr-ioctl-data))
                                (i2c-msg '(:struct i2c-msg))
                                (wbuf :uint8 datalen))
      (cffi:lisp-array-to-foreign wvec wbuf (list :array :uint8 datalen))
      (cffi:with-foreign-slots ((addr flags len buf) i2c-msg (:struct i2c-msg))
        (setf addr i2c-adr flags 0 len datalen buf wbuf))
      (cffi:with-foreign-slots ((msgs nmsgs) packets (:struct i2c-rdwr-ioctl-data))
        (setf msgs i2c-msg nmsgs 1))
      (osicat-posix:ioctl fd +i2c-rdwr+ packets)
      (unless (integerp i2c-dev) (osicat-posix:close fd)))))

(defun linux-i2c-read (i2c-adr read-len &key (i2c-dev "/dev/i2c-0"))
  (declare (type (unsigned-byte 16) i2c-adr read-len) (optimize safety))
  (let ((ret-buf (cffi:make-shareable-byte-vector read-len))
        (fd (if (integerp i2c-dev) i2c-dev
                (osicat-posix:open i2c-dev osicat-posix:o-rdwr))))
    (cffi:with-pointer-to-vector-data (bufptr ret-buf)
      (cffi:with-foreign-objects ((packets '(:struct i2c-rdwr-ioctl-data))
                                  (i2c-msg '(:struct i2c-msg)))
        (cffi:with-foreign-slots ((addr flags len buf) i2c-msg (:struct i2c-msg))
          (setf addr i2c-adr flags :i2c-m-rd len read-len buf bufptr))
        (cffi:with-foreign-slots ((msgs nmsgs) packets (:struct i2c-rdwr-ioctl-data))
          (setf msgs i2c-msg nmsgs 1))
        (osicat-posix:ioctl fd +i2c-rdwr+ packets)
        (unless (integerp i2c-dev) (osicat-posix:close fd))))))

(defun linux-i2c-eeprom-read (i2c-adr subadr read-len &key (i2c-dev "/dev/i2c-0"))
  "Uses Linux's 2019 I2C userspace API to read from a I2C device on the bus that behaves like an
EEPROM chip. (i.e. I2C start + 7bit addr + R/W bit + 16bit subadr + data byte(s) + I2C stop).
:i2c-bus accepts a number to specify which /dev/i2c-? device to open."
  (declare (type (unsigned-byte 16) i2c-adr subadr read-len) (optimize safety))
  (linux-i2c-write-then-read
   i2c-adr
   (list (ldb (byte 8 8) subadr) (ldb (byte 8 0) subadr))
   read-len
   :i2c-dev i2c-dev))

(defun linux-i2c-eeprom-write (i2c-adr subadr data &key (i2c-dev "/dev/i2c-0"))
  "Uses Linux's 2019 I2C userspace API to write to a I2C device on the bus that behaves like an
EEPROM chip. (i.e. I2C start + 7bit addr + R/W bit + 16bit subadr + data byte(s) + I2C stop).
:i2c-bus accepts a number to specify which /dev/i2c-? device to open."
  (declare (type (unsigned-byte 16) i2c-adr subadr)
           (optimize safety))
  (linux-i2c-write
   i2c-adr
   (concatenate '(vector (unsigned-byte 8)) (list (ldb (byte 8 8) subadr) (ldb (byte 8 0) subadr))
                data)
   :i2c-dev i2c-dev))




