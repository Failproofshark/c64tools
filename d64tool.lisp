(in-package :cl-user)
(defpackage :d64
  (:use :cl
        :cl-ppcre)
  (:export :initialize-bam
           :create-directory-entry
           :update-bam-entry
           :compile-disk))
(in-package :d64)

;;TODO FEATURE: perhaps expand this to possibly edit d64 files/act as a d64 file manager
(defconstant +standard-disk-size+
  174848
  "The standard size of a d64 file according to http://unusedino.de/ec64/technical/formats/d64.html")
(defconstant +total-tracks-in-standard-disk+
  35
  "The standard number of tracks of a d64 formatted disk according to http://unusedino.de/ec64/technical/formats/d64.html")
(defconstant +sector-size-in-bytes+ 
  256
  "Number of bytes in a sector according to http://unusedino.de/ec64/technical/formats/d64.html")

;; TODO determine if this still needs to be a table or simply a function 
(defvar +sectors-per-track-table+
  (let ((new-track-per-sector-table (make-array +total-tracks-in-standard-disk+ :element-type 'unsigned-byte)))
    (dotimes (track +total-tracks-in-standard-disk+)
      (cond ((and (>= track 0)
                  (<= track 16))
             (setf (aref new-track-per-sector-table track) 21))
            ((and (>= track 17)
                  (<= track 23))
             (setf (aref new-track-per-sector-table track) 19))
            ((and (>= track 24)
                  (<= track 29))
             (setf (aref new-track-per-sector-table track) 18))
            ((and (>= track 30)
                  (<= track 34))
             (setf (aref new-track-per-sector-table track) 17))))
    new-track-per-sector-table)  
  "The standard number of tracks per sector according to http://unusedino.de/ec64/technical/formats/d64.html")

(defstruct (disk (:constructor make-disk (disk-name)))
  "A struct that represents a disk image. BAM is represented as an array of unsigned-bytes. Directories are a list of unsigned-byte arrays. The contents is a hashmap of d64-files keyed by their on-disk file name"
  (disk-name "" :type string)
  (bam (initialize-bam disk-name) :type vector)
  ;;This is a list of unsigned-byte arrays. Used a list of arrays instead of a giant byte array to better support other disk types later. However I will probably switch to a giant array in the future and create functions to help manipulate array directories
  (files (make-hash-table :test equal) :type hash-table))

(defstruct d64-file 
  "A struct that represents a file on a d64-formatted-disk."
  (file-name :: :type string)
  (file-size 0 :type integer)
  ;; Should probably be a symbol
  (file-type "" :type string)
  ;; We should pre-chunk this for easier writing later
  (contents 'nil :type vector)
  (start-track 0 :type integer)
  (start-sector 0 :type integer)
  (directory-entry-number 0 :type integer)
  (directory-entry 'nil :type vector))

;; TODO by default take first 16 characters of file-name for the entry-name
;; TODO allow for raw-content (perhaps make path-to-file and raw-content be key arguments
(defun create-new-d64-file (file-type path-to-file entry-name start-track start-sector)
  (let ((new-d64-file (make-d64-file :file-name entry-name
                                     :file-type file-type
                                     ;; These two should be supplied based on availability according to the BAM
                                     :start-track start-track
                                     :start-sector start-sector))
        (collected-contents 'nil)
        ;; What does fill-pointer do again?
        (content-array (make-array 256 :element-type 'unsigned-byte))
        (file-length 0))
    (with-open-file (data path-to-file :element-type 'unsigned-byte)
      (loop byte = (read-byte data 'nil 'nil) while byte do
            ()))))

;; TODO write a function that reads a disk into a disk struct

;; TODO we need a function that records item to the BAM
;; TODO we need a function that records directory entries
(defun seek-to-track-sector (track sector)
  "Gives the byte offset represetingint the first position of a track/sector. Conventionally tracks are 1 indexed while sectors are 0 indexed and we follow that convention for consistency"
  (labels ((check-if-valid-track-sector-p (track sector)
             (and (> track 0)
                  (>= sector 0) 
                  (< sector (aref +sectors-per-track-table+ (- track 1)))))
           (number-of-sectors-in (track sector)
             (if (> track 1)                 
                 (+ (reduce #'+ (subseq +sectors-per-track-table+ 0 (- track 1))) sector)
                 sector)))
    ;; TODO raise condition otherwise
    (if (check-if-valid-track-sector-p track sector)
        (* +sector-size-in-bytes+ (number-of-sectors-in track sector)))))

;; TODO expand this to actually accomodate this for non PRG files
;; TODO Accomodate placing files on other tracks and consider the bam for locating sector to place file
(defun create-directory-entry (entry-number new-file-name)
  (if (and (simple-string-p new-file-name)
           (<= (length new-file-name) 16))
      (let ((track-of-next-directory-sector-offset 0)
            (sector-of-next-directory-sector-offset 1)
            (file-type-offset 2)
            (track-of-file-first-sector-offset 3)
            (sector-of-file-first-sector-offset 4)
            (filename-offset 5)
            (track-of-first-sector-of-side-sector-block-offset 20)
            (sector-of-first-sector-of-side-sector-block-offset 21)
            (rel-file-length-offset 22)
            (unused-block-offset 23)
            ;; file size is given in sectors low byte high byte order
            (file-size-offset 30)
            (new-entry (make-array 32 :element-type 'unsigned-byte :initial-element #x0)))
        
        ;; Technically the first two bytes should only be set if the directory spans multiple tracks (which is possible but unlikely) and is only filled out at the start of a directory track
        (if (= entry-number 1)
            (progn
              (setf (aref new-entry track-of-next-directory-sector-offset) #x12)
              (setf (aref new-entry sector-of-next-directory-sector-offset) #x04)))
        (setf (aref new-entry file-type-offset) #x82)
        ;; This should be a call to a function that retrieves the nearest possible available sector according to the BAM
        (setf (aref new-entry track-of-file-first-sector-offset) 1)
        (setf (aref new-entry sector-of-file-first-sector-offset) 0)
        (dotimes (i 16)
          (setf (aref new-entry (+ i filename-offset)) (if (>= i (length new-file-name))
                                                           #xa0
                                                           (char-code (char-upcase (aref new-file-name i))))))
        ;; TODO actually calcuate the file size or accept it as an argument
        (setf (aref new-entry file-size-offset) #x1)
        (setf (aref new-entry (+ 1 file-size-offset)) #x0)
        new-entry)
      ;; We should signal an error if the file name is longer than the maximum 16 characters or the filename is not a string
      ))

;; TODO we need a bam calculator to calculate the BAM entries
(defun initialize-bam (disk-name)
  (if (<= (length (string-trim '(#\Space #\tab #\Newline) disk-name)) 16)
      (let ((new-bam (make-array 256 :element-type 'unsigned-byte :initial-element #x0))
            (track-of-first-directory-sector-offset 0)
            (sector-of-first-directory-sector-offset 1)
            (disk-dos-version-type-offset 2)
            (start-of-BAM-entries-offset 4)
            (disk-name-offset 144)
            (disk-fill-one 160)
            (disk-id-offset 162)
            (dos-type-offset 165)
            (disk-fill-two 167))
        
        (setf (aref new-bam track-of-first-directory-sector-offset) #x12)
        (setf (aref new-bam sector-of-first-directory-sector-offset) #x1)
        (setf (aref new-bam disk-dos-version-type-offset) #x41)
        (setf (aref new-bam disk-fill-one) #xa0)
        (setf (aref new-bam (+ 1 disk-fill-one)) #xa0)
        ;; I dont know what the disk id actually does
        (setf (aref new-bam disk-id-offset) #x0)
        (setf (aref new-bam (+ 1 disk-id-offset)) #x1)
        ;; Technically disk-id is 2 bytes but this random byte is "usually" a0
        (setf (aref new-bam (+ 2 disk-id-offset)) #xa0)
        (setf (aref new-bam dos-type-offset) #x2a)
        
        (dotimes (i 16)
          (setf (aref new-bam (+ i disk-name-offset)) (if (>= i (length disk-name))
                                                          #xa0
                                                          (char-code (char-upcase (aref disk-name i))))))

        (dotimes (i 4)
          (setf (aref new-bam (+ i disk-fill-two)) #xa0))
        ;; We mark all items empty except for track 18 sectors one and two
        ;; refactor this to make it part of the sectors-per-track table we originally created
        
        (loop for current-track from 1 to 35 
              for current-byte from 0 to 139 by 4 do
                (cond ((and (>= current-track 1)
                            (<= current-track 17))
                       (progn (setf (aref new-bam (+ current-byte start-of-BAM-entries-offset)) 21)
                              (setf (aref new-bam (+ 1 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 2 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 3 current-byte start-of-BAM-entries-offset)) #xf8)))
                      ((and (>= current-track 18)
                            (<= current-track 24))
                       (progn (setf (aref new-bam (+ current-byte start-of-BAM-entries-offset)) 19)
                              (setf (aref new-bam (+ 1 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 2 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 3 current-byte start-of-BAM-entries-offset)) #xe0)))
                      ((and (>= current-track 25)
                            (<= current-track 30))
                       (progn (setf (aref new-bam (+ current-byte start-of-BAM-entries-offset)) 18)
                              (setf (aref new-bam (+ 1 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 2 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 3 current-byte start-of-BAM-entries-offset)) #xc0)))
                      ((and (>= current-track 31)
                            (<= current-track 35))
                       (progn (setf (aref new-bam (+ current-byte start-of-BAM-entries-offset)) 17)
                              (setf (aref new-bam (+ 1 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 2 current-byte start-of-BAM-entries-offset)) #xff)
                              (setf (aref new-bam (+ 3 current-byte start-of-BAM-entries-offset)) #x80)))))  
        ;; The 18th track (byte 71 in the bam) is a special case since the first and second sector are specially reserved
        ;; for the BAM and first (and subsequent) sector(s) contain the directories on the disk
        ;; (determined using 4 (number of bytes per directory entry) * 18 (18th track) - 1 (compensate for 0-index)
        (setf (aref new-bam 71) 17)
        (setf (aref new-bam 72) #x2f)
        new-bam))
  ;;Should signal a condition if the disk-name is larger than 16
  )

;; TODO perhaps create a integer->bit-vector
;; TODO move this to some utility file some day
(defun bit-vector->integer (bit-vector)
  (loop for bit from 0 to (- (length bit-vector) 1) 
        for power from (- (length bit-vector) 1) downto 0 sum
                                                    (* (aref bit-vector bit) (expt 2 power))))

;; TODO have this support freeing up a block? Right now we only support when we wish to write to a track and thus decrease the currently available number of sectors in a track
;; TODO have an error check to ensure the sectors exists (e.g. track 1 only has 21 sectors therefore we should not be able to set track 22)
(defun update-bam-entry (bam track sector)
  (let* ((bytes-per-track 4)
         (start (* bytes-per-track track))
         (sectors-0-7 (+ 1 start))
         (sectors-8-15 (+ 2 start))
         ;; This last byte needs to be dealt with in a special way since not there are a few bytes unused given the uneven number of tracks
         (sectors-16-23 (+ 3 start)))
    
    (decf (aref bam start))
    ;; 1 means block is available 0.  bit-field is an integer that represents 8 bits
    (labels ((turn-off-bit (bit-field bit)
               (let ((bit-mask (make-array 8 :element-type 'bit :initial-element 1)))
                 (setf (aref bit-mask bit) 0)
                 (logand (bit-vector->integer bit-mask) bit-field))))
      (cond ((and (>= sector 0)
                  (<= sector 7))
             (setf (aref bam sectors-0-7) (turn-off-bit (aref bam sectors-0-7) sector)))
            ((and (>= sector 8)
                  (<= sector 15))
             (setf (aref bam sectors-8-15) (turn-off-bit (aref bam sectors-8-15) (- sector 8))))
            ((and (>= sector 16)
                  (<= sector 23))
             (setf (aref bam sectors-16-23) (turn-off-bit (aref bam sectors-16-23) (- sector 16))))))))

;; mainly used for debugging
;; TODO should try to generalize this one day to generate fixed sized byte table readings
;; TODO just replace array-slice with subseq
(defun pretty-print-bam-contents (disk)
  (labels ((array-slice (array-to-slice start-index last-index)
             (loop for item from start-index to last-index collect (aref array-to-slice item))))
    (format t "   ~{0~x ~}~%" (loop for i from 0 to 15 collect i))
    (loop for i from 0 to 255 by 16
          for row-counter from 0 to 15 do
            (format t "~x0 ~{~2,'0x ~}~%" row-counter (array-slice (disk-bam disk) i (+ 15 i))))))

;; TODO rename this to create-new-disk
(defun initialize-disk ()
  (make-array +standard-disk-size+ :element-type 'unsigned-byte :initial-element #x0))

(defun write-to-disk (disk-byte-array track sector interleave content)
  (let ((start-offset (seek-to-track-sector track sector)))
    (loop for index from 0 upto (- (length content) 1) do
          (setf (aref disk-byte-array (+ start-offset index)) (aref content index)))))

;; TODO determine number of bytes and divide it among track/sectors as evenly as possible interleaving sectors of 10 (implementation note will need to check if at last sector of track if so need to move to next).

;; TODO should accept the BAM and update the bam properly
;; TODO need to properly support file contents larger than 254 bytes aka interleaving several sectors properly
;; TODO write procedure skip first two bytes. If we hit 256 write the first two bytes to be the next track sector otherwise 00 number of bytes remaining
;; TODO we'll need to determine if we need to move to the next track due to no more remaining sectors in the track
;; TODO NOTE technically we only need to keep track of the current track and sector that way when it comes to write the next track and sector of our next block or book ending remainder of bytes simply need to seek-to-track-sector then set that byte and the one following
;; TODO need to error out if there are no available blocks (have to check against the BAM
(defun write-file-to-disk (start-track start-sector disk content-file)
  (with-open-file (contents content-file :direction :input :element-type 'unsigned-byte)
    (let ((byte-count 0))
      (loop with byte-offset = (seek-to-track-sector start-track start-sector)
            for byte = (read-byte contents 'nil 'nil) while byte do
              (incf byte-count)
              (setf (aref disk byte-offset) byte)
              (incf byte-offset))
       ;; We need to write our book ender bytes
      (setf (aref disk (seek-to-track-sector start-track start-sector)) 0)
      (setf (aref disk (+ 1 (seek-to-track-sector start-track start-sector))) byte-count))))

;; TODO should also accept the BAM to determine if the directory entry has
(defun write-directory-entry (disk directory-entries)
  (let* ((directory-track-sector-start (seek-to-track-sector 18 1))
         (bytes-per-entry 32)
         (last-byte (+ directory-track-sector-start (* bytes-per-entry (length directory-entries)) -1)))
    (loop for disk-byte-index from directory-track-sector-start to last-byte by bytes-per-entry
          for entry in directory-entries do
            (loop for byte across entry
                  for byte-index from 0 to (- bytes-per-entry 1) do
                    (setf (aref disk (+ disk-byte-index byte-index)) byte)))))

(defun write-bam (disk-byte-array bam)
  (let ((bam-track-sector-start (seek-to-track-sector 18 0)))
    (loop for byte across bam
          for disk-index from 0 to (- (length bam) 1) do
            (setf (aref disk-byte-array (+ bam-track-sector-start disk-index)) byte))))

(defun create-d64-image (disk)
  (let ((new-disk (make-array +standard-disk-size+ :element-type 'unsigned-byte :initial-element #x0)))
    (write-bam new-disk (disk-bam disk))
    (loop for file-name being the hash-key of (disk-files disk) do
      (write-)))
  (write-directory-entry disk *directory-entries*)
  (write-bam disk bam)
  (save-disk-image disk-name disk))

;; TODO have this accept: a series of directory entries and write them accordingly,

;; TODO prior to writing a prg to the disk need to ensure first two bytes is either the next sector continuing the rest of the file's contents or 00 followed how much of the sector is used (remaining bytes i think)
