;;; ndmacro.el --- macro that support counting
;;
;; Version: $Id: ndmacro.el, v 0.0 2017/07/07 $
;; last updated : 2012-02-07

(eval-when-compile (require 'cl))

(defvar *ndmacro-repeat-count* 0)
(defvar *ndmacro-previous-steps* nil)

;; from On Lisp utility
(defun ndmacro:util-group (source n)
  (if (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

;; (ndmacro:list-shift '(1 2 3) '(4 5 6))
;; => ((1 2) (3 4 5 6))
(defun ndmacro:list-shift (list1 list2)
  (list (reverse (cdr (reverse list1)))
        (cons (car (reverse list1)) list2)))

(defun ndmacro:is-number (x)
  (if (and (numberp x)
           (<= 48 x)
           (<= x 57))
      x nil))

(defun ndmacro:is-not-number (x)
  (if (and (numberp x)
           (<= 48 x)
           (<= x 57))
      nil x))

;; (ndmacro:seq-prefix-match
;;  '(2 1 0 "A" 5 4 3)
;;  '(2 1 0 "A" "H" "O"))
;; => (2 1 0 "A")
(defun ndmacro:seq-prefix-matched (lst1 lst2)
  (let ((idx 0))
    ;; (message "-1:%s" lst1)
    ;; (message "-2:%s" lst2)
    (while (equal (nth idx "%s" "%s" lst1)
                  (nth idx "%s" "%s" lst2))
    ;; (message "-3:%s" lst1)
    ;; (message "-4:%s" lst2)

      (incf idx))
    (subseq lst1 0 idx)))

;; (multiple-value-list (ndmacro:search-loop
;;   '("a" "b" "a" "b" "c" "c"
;;     "a" "b" "a" "b" "c" "c" "d" "e" "f" "g")))
;; => ((("a" "b" "a" "b" "c" "c") ("a" "b" "a" "b" "c" "c")) 0)
;; (ndmacro:search-loop
;;   '("a" "b" "a" "b" "c" "-"
;;     "a" "b" "a" "b" "c" "c" "d" "e" "f" "g"))
;; => ((("a" "b") ("a" "b")) 0)
;; (ndmacro:search-loop
;;  '("a" "b"
;;    "a" "b"))
;; => ((("a" "b") ("a" "b")) 0)
;; (ndmacro:search-loop
;;  '("a" "b"
;;    "a" "b" "a" "b"))
;; => ((("a" "b") ("a" "b") ("a" "b")) 0)
;; (ndmacro:search-loop
;;  '("a" "b" "a" "b"
;;    "a" "b" "a" "b"))
;; => ((("a" "b" "a" "b") ("a" "b" "a" "b")) 0)
;; (ndmacro:search-loop
;;  '(49 48 49 44
;;    49 48 50 44
;;    49 48 51 44))
;; => (((49 48 49 44) (49 48 50 44) (49 48 51 44)) 0)
;; (ndmacro:search-loop
;;  '(2 1 0 "A" 5 4 3 2 1 0 "A" "H" "O"))
;; => (((5 4 3 2 1 0 "A") (5 4 3 2 1 0 "A")) 4)

;; lstを半分にして、前半分と後半分が先頭一致するか見る。
;; 一致しなければ前半分の一番後ろを後半分の先頭に持ってきて比較。以降繰り返し。
(defun ndmacro:search-loop (lst)
  (let* ((center-pos (floor (length lst) 2))
         (list1 (subseq lst 0 center-pos))
         (list2 (subseq lst center-pos))
         shifted)
    ;; 数字[0-9]同士は同じものとみなす。あとから差を出して連番生成できるように。
    (setq list1 (mapcar 'ndmacro:is-not-number list1))
    (setq list2 (mapcar 'ndmacro:is-not-number list2))

    (while (and list1 ;; list1が残ってれば続ける
                ;; list1,list2が異なれば続ける
                (not (equal list1
                            (subseq list2 0 (length list1)))))
      (setq shifted (ndmacro:list-shift list1 list2)
            list1   (first  shifted)
            list2   (second shifted))
      )
    ;; ループが終わったら,list1は｛空っぽ｜list2との先頭一致リスト｝のどっちか。

    (cond (list1
           (let ((begin 0)
                 (end   (length list1)))
             (while (equal list1 (subseq list2
                                         (+ begin (length list1))
                                         (+ end   (length list1))))
               (setq begin (+ begin (length list1))
                     end   (+ end   (length list1))))
             ;; 繰り返しの全体と、途中までの場合何桁目まで入力しているか？を返す
             (values (ndmacro:util-group (subseq lst 0 (+ end (length list1)))
                                         (length list1))
                     0
                     ;; (subseq lst (+ begin (length list1))
                     ;;             (+ end (length list1)))
                     )
             ))
          (t
          ;; 完全一致の繰り返しがなければ途中までの入力から予測
           (ndmacro:predict-repeat lst)))))

;; (ndmacro:predict-repeat
;;  '(2 1 0 "A" 5 4 3 2 1 0 "A" "H" "O"))
;; => ((("A" 0 1 2 3 4 5) ("A" 0 1 2 3 4 5)) 4)
(defun ndmacro:predict-repeat (lst)
  (let* ((lst lst) ; --time-->
         (latest-val-pos (position (first lst) lst :start 1))
         repeat-start-pos
         repeat-end-pos
         )
    (setq repeat-end-pos (length (ndmacro:seq-prefix-matched
                                  (subseq lst 0 latest-val-pos)
                                  (subseq lst latest-val-pos))))
    (setq repeat-start-pos (+ latest-val-pos
                              repeat-end-pos))
    (cons (list (subseq lst
                        repeat-end-pos
                        repeat-start-pos)
                (append (subseq lst repeat-end-pos latest-val-pos)
                                 (subseq lst 0 repeat-end-pos)))
          (list repeat-end-pos))))


;; (ndmacro:split-seq-if 'identity '(49 51 nil 49 52 nil));=> ((49 51) (49 52))
;; (ndmacro:split-seq-if 'identity '(nil 49 51 nil 49 52 nil));=> ((49 51) (49 52))
;; (ndmacro:split-seq-if 'identity '(nil 49 51 nil 49 52));=> ((49 51) (49 52))
(defun ndmacro:split-seq-if (test lst)
  (let (beg end)
    (when (setq beg (position-if     test lst :start 0))
      (setq end (or (position-if-not test lst :start beg) (length lst)))
      (cons (subseq lst beg end)
            (ndmacro:split-seq-if test (subseq lst end))))))

;; (ndmacro:position-subseq '(nil 49 51 nil 49 52) '(49 51));; => 1
;; (ndmacro:position-subseq '(nil 49 51 nil 49 52) '(49 52));; => 4
(defun ndmacro:position-subseq (lst sub)
  (let ((pos 0)
        (continue-flag t)
        res)
    (while (and continue-flag
                (setq pos (position (first sub) lst :start pos)))
      (cond ((equal (subseq lst pos (+ pos (length sub)))
                    sub)
             (setq res pos)
             (setq continue-flag nil))
            (t
             (incf pos))))
    res))

;; (ndmacro:get-numbers-and-position
;;  '(nil 49 48 51 nil 49 51 nil nil))
;; => ((1 3 103) (5 2 13)) => 1の位置から３桁分105, 5の位置から2桁分13がある。
(defun ndmacro:get-numbers-and-position (lst)
  (let* ((splitted (ndmacro:split-seq-if 'identity lst))
         (numbers (mapcar (lambda (l)
                             (apply 'concatenate 'string
                                    (mapcar 'string l)))
                          splitted)))
    (mapcar* 'list
             (mapcar #'(lambda (sub) (ndmacro:position-subseq lst sub))
                     splitted)
             (mapcar #'(lambda (n) (length n)) numbers)
             (mapcar 'string-to-number numbers))))

;; (ndmacro:get-incremented-sequence
;; '((return 65 53 49 44 52 48 49);"104,15A"
;;   (return 65 51 49 44 51 48 49);"103,13A"
;;   (return 65 49 49 44 50 48 49);"102,11A"
;;   ))
;; => (49 48 53 44 49 55 65 return);"105,17A"
(defun ndmacro:get-incremented-sequence (lst)
  (setq lst (mapcar 'reverse lst))
  (let* (;; 数字以外nilに変えちゃう
         (lst1  (ndmacro:get-numbers-and-position
                 (mapcar 'ndmacro:is-number (nth 0 lst))))
         (lst2 (ndmacro:get-numbers-and-position
                (mapcar 'ndmacro:is-number (nth 1 lst))))
         (next-number
          (mapcar* 'list ; 位置情報もくっつけとく。
                   lst1
                   (mapcar* '+ ; 足すと次の数字になって↑↑
                            (mapcar 'third lst1)
                            (mapcar (lambda (e) (* *ndmacro-repeat-count* e)); 連続実行の場合は実行回数をかけて↑↑
                                    (mapcar* '- ; 差を出して↑↑
                                             (mapcar 'third lst1)
                                             (mapcar 'third lst2)))
                            )))
         (result-seq (copy-list (first lst))))
    (dolist (l next-number) ;;繰り返し1つの中に複数数字がある場合に備えて
      (let ((chars (map 'list 'identity (substring (format "000000000000000000%d" (max 0 (second l))) ;;桁数維持
                                                   (- (cadar l))
                                                   ))))
        (dotimes (n (cadar l))
          (setf (nth (+ n (caar l)) result-seq) (nth n chars)))))

    result-seq ;; ←これが連番の増えたver
    ))

(defun ndmacro:get-key-list ()
  (let ((lst (reverse (append (recent-keys) nil)))
        loop-elm loop-all input-count result match-pos)
    ;; 繰り返しとみなさないものを除外：
    ;; 直近のndmacroキーを除外した上で、
    (while (and (setq match-pos (position last-command-event lst :test 'equal))
                (= match-pos 0))
      (setq lst (cdr lst)))
    ;; 最後にndmacroのキーを押した時以降の入力を探索対象に。
    ;; => ndmacroキーを跨いで繰り返しとみなさない
    (setq lst (subseq lst 0
                      (position last-command-event lst :test 'equal)))
    ;; 繰り返しを探す
    (multiple-value-setq (loop-all input-count)
      (ndmacro:search-loop lst))

    (setq loop-elm (reverse (nth -1 loop-all)))

    ;;数字が入ってたら連番増やす
    (setq result (cond ((find-if 'ndmacro:is-number loop-elm)
                        ;(message "%s" loop-all)
                        (ndmacro:get-incremented-sequence loop-all))
                       (t
                        loop-elm)))
    ;;繰り返しを予測した場合の最初のC-tの時のみ繰り返し要素の一部のみ実行
    (cond ((and (= *ndmacro-repeat-count* 1)
                (< 0 input-count))
           (nthcdr input-count result))
          (t result))))

(defun ndmacro ()
  (interactive)
  (cond ((equal real-last-command this-command)
         (incf *ndmacro-repeat-count*))
        (t
         (setq *ndmacro-repeat-count* 1)))
  ;; (message "lc:%s tc:%s lce:%c tck:%s lcc:%c lie:%c lef:%s"
  ;;                real-last-command this-command last-command-event
  ;;                (this-command-keys)
  ;;                last-command-char
  ;;                last-input-event
  ;;                last-event-frame
  ;;                )
  (let ((lst (ndmacro:get-key-list)))
    (cond ((not lst)
           (message "繰り返しが見つかりません"))
          ((not nil)
           ;(message "繰り返しはこれです：%s" lst)
           (execute-kbd-macro (apply 'vector lst))))))

;;; TODO:
;;;
;;; - win, macで確認すること
;;; - dmacroの規則２ (XYXのパターン)に対応させる?

(provide 'ndmacro)
;;; ndmacro.el ends here
