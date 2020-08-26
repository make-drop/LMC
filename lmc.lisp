;;; Marco Caspani
;;; Marco Gatti     


;;; LMC SIMULATOR functions (first part)

;; Returns the instruction fetched from the ram memory
(defun fetch (state)
  (let ((pc (get-pc state)))
    (cons state (mem-load state pc))))

;; Returns a list cell with the state, instruction to execute and the xx value
(defun decode (cell)  ; Input: cons cell of state and instruction to execute
  (let ((state (car cell))
        (opcode (floor (/ (cdr cell) 100)))
        (xx-value (mod (cdr cell) 100)))

    (cond
      ((equal opcode 1) (list state 'ins-addition xx-value))
      ((equal opcode 2) (list state 'ins-subtraction xx-value))
      ((equal opcode 3) (list state 'ins-instruction-store xx-value))
      ((equal opcode 5) (list state 'ins-load xx-value))
      ((equal opcode 6) (list state 'ins-branch xx-value))
      ((equal opcode 7) (list state 'ins-branch-if-zero xx-value))
      ((equal opcode 8) (list state 'ins-branch-if-positive xx-value))
      ((equal opcode 0) (list state 'ins-halt xx-value))
      ((equal (cdr cell) 901) (list state 'ins-input NIL))
      ((equal (cdr cell) 902) (list state 'ins-output NIL))
      (T (list state 'ins-illegal-opcode NIL)))))

;; Decoded instruction = (state instruction-function xx)
;; Calls the instruction function and returns the value
(defun execute-instruction (decoded-instruction)
  (let ((state (first decoded-instruction))
        (ins-function (second decoded-instruction))
        (ins-xx-value (third decoded-instruction)))
    (funcall ins-function state ins-xx-value)))

;; INSTRUCTIONS FUNCTIONS

;; The addition function (ADD)
;; Performs the addition acc = acc + xx
(defun ins-addition (state xx-value)
  (let ((sum (+ (get-acc state) (mem-load state xx-value))))
    (if (> sum 999)
      (list 'state :ACC (mod sum 1000)
                  :PC (mod (+ 1 (get-pc state)) 100)
                  :MEM (get-mem state)
                  :IN (get-input-queue state)
                  :OUT (get-output-queue state)
                  :FLAG 'flag)
      (list 'state :ACC (mod sum 1000)
                  :PC (mod (+ 1 (get-pc state)) 100)
                  :MEM (get-mem state)
                  :IN (get-input-queue state)
                  :OUT (get-output-queue state)
                  :FLAG 'noflag))))

;; The subtraction function (SUB)
;; Performs the subtraction acc = acc - xx
(defun ins-subtraction (state xx-value)
  (let ((sub (- (get-acc state) (mem-load state xx-value))))
    (if (< sub 0)
      (list 'state :ACC (mod sub 1000)
                  :PC (mod (+ 1 (get-pc state)) 100)
                  :MEM (get-mem state)
                  :IN (get-input-queue state)
                  :OUT (get-output-queue state)
                  :FLAG 'flag)
      (list 'state :ACC (mod sub 1000)
                  :PC (mod (+ 1 (get-pc state)) 100)
                  :MEM (get-mem state)
                  :IN (get-input-queue state)
                  :OUT (get-output-queue state)
                  :FLAG  'noflag))))

;; The store function (STA)
;; Returns the state with the mem with the  value stored
(defun ins-instruction-store (state xx-value)
  (let ((mem (copy-list (get-mem state)))) ; not to produce side effects
    (setf (nth xx-value mem) (get-acc state))
      (list 'state :ACC (get-acc state)
                :PC (mod (+ 1 (get-pc state)) 100)
                :MEM mem
                :IN (get-input-queue state)
                :OUT (get-output-queue state)
                :FLAG (get-flag state))))

;; Illegal opcode : 4 -> fail the computation
(defun ins-illegal-opcode (state xx-value) NIL)

;; The load instruction (LDA)
(defun ins-load (state xx-value)
  (list 'state :ACC (mem-load state xx-value)
              :PC (mod (+ 1 (get-pc state)) 100)
              :MEM (get-mem state)
              :IN (get-input-queue state)
              :OUT (get-output-queue state)
              :FLAG (get-flag state)))

;; The branch function (BRA)
;; Returns state but with the PC changed to xx
(defun ins-branch (state xx-value)
  (list 'state :ACC (get-acc state)
              :PC xx-value
              :MEM (get-mem state)
              :IN (get-input-queue state)
              :OUT (get-output-queue state)
              :FLAG (get-flag state)))

;; The branch if zero instruction (BRZ)
;; changes the PC if ACC = 0 and flag = noflag
(defun ins-branch-if-zero (state xx-value)
  (if (and (equal (get-acc state) 0) (equal (get-flag state) 'noflag))
    (ins-branch state xx-value)
    (list 'state :ACC (get-acc state)
                :PC (mod (+ 1 (get-pc state)) 100)
                :MEM (get-mem state)
                :IN (get-input-queue state)
                :OUT (get-output-queue state)
                :FLAG (get-flag state))))

;; The branch if positive instruction (BRP)
;; changes the PC if flag = noflag
(defun ins-branch-if-positive (state xx-value)
  (if (equal (get-flag state) 'noflag)
    (ins-branch state xx-value)
    (list 'state :ACC (get-acc state)
                :PC (mod (+ 1 (get-pc state)) 100)
                :MEM (get-mem state)
                :IN (get-input-queue state)
                :OUT (get-output-queue state)
                :FLAG (get-flag state))))

;; Returns an halted state with the PC value that hasn't been incremented
(defun ins-halt (state xx-value)
  (list 'halted-state :ACC (get-acc state)
              :PC (get-pc state)
              :MEM (get-mem state)
              :IN (get-input-queue state)
              :OUT (get-output-queue state)
              :FLAG (get-flag state)))

;; Input function returns a state with acc = car of input-queue
;; and removes the first element of input-queue
(defun ins-input (state xx-value)
  (if (null (get-input-queue state))
    NIL
    (list 'state :ACC (car (get-input-queue state))
                :PC (mod (+ 1 (get-pc state)) 100)
                :MEM (get-mem state)
                :IN (cdr (get-input-queue state))
                :OUT (get-output-queue state)
                :FLAG (get-flag state))))

;; Output function adds acc to output-queue
;; and returns the state with that queue
(defun ins-output (state xx-value)
  (list 'state :ACC (get-acc state)
              :PC (mod (+ 1 (get-pc state)) 100)
              :MEM (get-mem state)
              :IN (get-input-queue state)
              :OUT (append (get-output-queue state) (list (get-acc state)))
              :FLAG (get-flag state)))

;; Returns execute(decode(fetch-instruction(state)))
(defun one-instruction (state)
  (cond
    ((null state) NIL)
    ((not (equal (length (get-mem state)) 100)) NIL)
    ((is-halted state) NIL)
    (T (let ((whole-instruction (compose 'execute-instruction 'decode 'fetch)))
      (funcall whole-instruction state)))))

;; Calls one-instruction till the computation stops with an halted state
(defun execution-loop (state)
  (let ((new-state (one-instruction state)))
    (if (is-halted state)
      (get-output-queue state)
      (if (is-halted new-state)
        (get-output-queue new-state)
        (execution-loop new-state)))))

;; UTILITY FUNCTIONS

;; Functions to return components of the state
(defun get-acc (state)
  (getf (cdr state) :acc))
(defun get-pc (state)
  (getf (cdr state) :pc))
(defun get-mem (state)
  (getf (cdr state) :mem))
(defun get-input-queue (state)
  (getf (cdr state) :in))
(defun get-output-queue (state)
  (getf (cdr state) :out))
(defun get-flag (state)
  (getf (cdr state) :flag))

;; returns the value that is contained at memcell in state
(defun mem-load (state memcell)
  (let ((mem (get-mem state)))
     (nth memcell mem)))

;; Returns T if the state of the lmc is an halted-state
(defun is-halted (state)
 (if (equal (car state) 'state) NIL T))

;; T if the string is empty
(defun is-empty (string)
  (equal string ""))

;; Alias of write-to-string
(defun to-string (number)
  (write-to-string number))

;; Compose composes a list of functions (f1 f2 ... fn)
;; Returns the function f1(f2(...(fn(x))..)
(defun compose (func1 &rest funcs)
  (if (null (car (flatten funcs)))
    (lambda (x) (funcall func1 x))
    (let ((func2 (compose (car (flatten funcs)) (cdr (flatten funcs)))))
      (lambda (y) (funcall func1 (funcall func2 y))))))

;; Flattens a list at the first level
(defun flatten (l)
  (cond ((null l) NIL)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (T (append (flatten (car l)) (flatten (cdr l))))))

;; Split a string where there are space characters and turns it into a list
(defun split-string (string)
  (reverse (split-string-iter string NIL)))
(defun split-string-iter (string acc)
  (let ((space-position (position #\Space string)))
    (if (null space-position)
      (append (list string) acc)
      (let (
            (new-string (subseq string (+ space-position 1) (length string)))
            (new-acc (append (list (subseq string 0 space-position)) acc))
           )
        (split-string-iter new-string new-acc)))))

;; Compresses any consecutive spaces
(defun compress-spaces-list (parts)
  (if (null parts)
    ""
    (let ((first-part
           (concatenate 'string (string-trim
                                 '(#\Space #\Tab #\Newline) (car parts)) " "))
          (second-part
           (compress-spaces-list (cdr parts))))
      (string-trim
       '(#\Space #\Tab #\Newline) (concatenate
                                   'string first-part second-part)))))

;; Needed to remove multiple occurrences of spaces in strings
;; Like "      string1  string2     string3"
(defun compress-spaces (string)
  (let ((parts (split-string string)))
    (compress-spaces-list parts)))

;; ASSEMBLER functions (second part)

;; Read all the lines from a file-stream
(defun read-lines (file-stream lines)
  (let ((line (read-line file-stream NIL)))
    (if (and (null line) (close file-stream))
      (values lines)
      (read-lines file-stream (append lines (list line))))))

;; Read all the file at the path filename
(defun read-file (filename)
  (let ((file-stream (open filename :if-does-not-exist :error)))
    (read-lines file-stream NIL)))

;; Get the position of a comment in an instruction string
(defun get-comment-position (string offset)
  (if (>= (length string) 2)
    (if (equal (subseq string 0 2) "//")
      (values offset)
      (progn
        (let ((new-string-to-check (subseq string 1 (length string))))
          (get-comment-position new-string-to-check (+ offset 1)))))
    (values NIL)))

;; Removes the comment from one instruction
(defun remove-comment (string)
  (let ((comment-position (get-comment-position string 0)))
    (if (null comment-position)
      string
      (subseq string 0 comment-position))))

;; Removes comments from every line of the code
(defun remove-comments (code)
  (mapcar 'remove-comment code))

;; Removes useless spaces
(defun trim-code (code)
  (let ((no-spaces (mapcar (lambda (e)
                             (funcall
                              #'string-trim '(#\Space #\Tab #\Newline) e))
                           code)))
    (mapcar #'compress-spaces no-spaces)))

;; removes empty lines of codes
(defun remove-empty-lines (code) ; cell = (code . labels)
  (remove-if 'is-empty code))

;; Returns the label if it exists in line
(defun get-label (line)
  (let ((string-parts (split-string line)))
    (cond
      ((equal (length string-parts) 0) (values NIL))
      ((equal (length string-parts) 1) (values NIL))
      ((equal (length string-parts) 2)
        (if (or (equalp (cadr string-parts) "INP")
                (equalp (cadr string-parts) "OUT")
                (equalp (cadr string-parts) "HLT")
                (equalp (cadr string-parts) "DAT"))
          (car string-parts)
          NIL))
      ((equal (length string-parts) 3) (values (car string-parts))))))

;; Returns a list of the labels associated to the line number
(defun get-labels (code &optional (offset 0))
  (if (null code)
    NIL
    (let ((label (list (cons (get-label (car code)) offset))))
      (if (null (car (car label)))
        (get-labels (cdr code) (+ offset 1))
        (append label (get-labels (cdr code) (+ offset 1)))))))

;; Returns a cons cell with code and the labels-list (code . labels-list)
(defun get-code-and-labels (code)
  (cons code (get-labels code)))

;; Removes a label from the source-code
(defun remove-label (source-code label)
  (let ((code (copy-list source-code))
        (label-position (cdr label)) ; get the position of the label to remove
        (line (nth (cdr label) source-code))) ; get the line where the label is
    (let ((new-line (subseq line (+ 1 (position #\Space line)) (length line))))
      (progn
        (setf (nth label-position code) new-line)
        (values code)))))

;; Cleans the code by removing the labels
(defun remove-labels (code labels-list)
    (if (null labels-list)
      code
      (let ((new-code (remove-label code (car labels-list))))
        (remove-labels new-code (cdr labels-list)))))

;; Removes labels from the code and returns (code . labels-list)
(defun get-code-without-labels (cell) ; cell = (code . labels)
  (let ((code (car cell)) (labels-list (cdr cell)))
    (cons (remove-labels code labels-list) labels-list)))

;; Given the name of the label and the labels-list
;; This function returns the position inside the code where the label is
(defun get-label-position (label labels-list)
  (if (null labels-list)
    NIL
    (if (equalp label (car (car labels-list)))
      (cdr (car labels-list))
      (get-label-position label (cdr labels-list)))))

;; Returns the prefix of the instruction code the lmc can run
(defun get-instruction-code (ins)
  (cond
    ((equalp ins "DAT") 0)
    ((equalp ins "ADD") 1)
    ((equalp ins "SUB") 2)
    ((equalp ins "STA") 3)
    ((equalp ins "LDA") 5)
    ((equalp ins "BRA") 6)
    ((equalp ins "BRZ") 7)
    ((equalp ins "BRP") 8)))

;; Returns the prefix of the instruction code the lmc can run
;; If ins does not take arguments
(defun get-instruction-code-len1 (ins)
  (cond
    ((equalp ins "DAT") 0)
    ((equalp ins "HLT") 0)
    ((equalp ins "INP") 901)
    ((equalp ins "OUT") 902)))

;; returns the 3 digits number code of the instruction translated from
;; the instruction name string and the xx value argument
(defun concatenate-inscode-xx (ins xx)
  (if (equal xx "NIL")
    NIL ; The label hasn't been found in the labels list
    (if (< (parse-integer xx) 10)
      (parse-integer
        (concatenate 'string
          (write-to-string (get-instruction-code ins))
          "0"
          (write-to-string (parse-integer xx))) :junk-allowed T)
      (parse-integer
        (concatenate 'string
          (write-to-string (get-instruction-code ins))
          (write-to-string (parse-integer xx))) :junk-allowed T))))

;; Translate a string instruction into its instruction code (3 digits)
(defun translate-instruction (line labels-list)
  (let ((parts (split-string line)))
    (cond
      ((equal (length parts) 1) (get-instruction-code-len1 (car parts)))
      ((equal (length parts) 2)
        (if (null (parse-integer (cadr parts) :junk-allowed T))
          ; If there is a label
          (if (not (equalp (car parts) "dat")) ; Exclude the case "dat label"
            (concatenate-inscode-xx
              (car parts)
              (to-string (get-label-position (cadr parts) labels-list)))
            NIL)
          ; If there is no label in the instruction
          (concatenate-inscode-xx (car parts) (cadr parts)))))))

;; Translates the whole code into machine-code
(defun translate-to-machine-code (code-labels)
  (let ((code (car code-labels)) (labels-list (cdr code-labels)))
    (mapcar (lambda (e) (funcall 'translate-instruction e labels-list)) code)))

;; If some errors occurred ignores the generated code and returns NIL
(defun final-code-check (code)
  (if (null (position NIL code))
    code ; there are no "NIL" instruction in the code (successfull translation)
    NIL))

;; Fill the memory with zeros if the code is less than 100 lines long
(defun fill-mem-with-zeros (code)
  (if (null code)
    NIL
    (if (> (length code) 99)
      code
      (fill-mem-with-zeros (append code '(0))))))

;; Assembler of the lmc
(defun lmc-load (filename)
  (let ((whole-function (compose 'fill-mem-with-zeros
                                 'final-code-check
                                 'translate-to-machine-code
                                 'get-code-without-labels
                                 'get-code-and-labels
                                 'remove-empty-lines
                                 'trim-code
                                 'remove-comments
                                 'read-file)))

       (funcall whole-function filename)))

;; Loads a program written in lmc assembly and runs it with
;; a specific input-queue taken as an argument
(defun lmc-run (filename input-queue)
  (let ((machine-code (lmc-load filename)))
    (if (null machine-code)
      NIL
      (execution-loop
        (list 'state :acc 0
                     :pc 0
                     :mem machine-code
                     :in input-queue
                     :out ()
                     :flag 'noflag)))))
