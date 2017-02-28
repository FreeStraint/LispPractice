
;Question 1
;This function will determine if the given element Y is contained in the list X
;If the given list X is not null, first compare the first element of X to Y.
;if equals, then return T
;else call the function with input of X without the first element and y
;If there is a match, return T, otherwise return NIL 
(defun xmember (X Y)
	(if (equal X NIL)
		NIL
		(if (equal (car X) Y) 
			T
			(xmember (cdr X) Y)) 
		)
	)

;Question 2
;This function is to extract every atom from a nested sublist and append them into a single list
;For every element in x, if the element is an atom, then append to the list
;if the element is a list, then flatten the element until it retrieve an atom and append it to the list
;Testcases
; >(flatten '(a (b c) d))
; (a b c d)

; >(flatten '((((a))))) 
; (a)

; >(flatten '(a (b c) (d ((e)) f)))
; (a b c d e f)
(defun flatten (X)
	(if (equal X NIL)
		NIL
		(if (atom(car X))
			(cons (car X) (append (flatten (cdr X))))
			(append (flatten (car X)) (flatten (cdr X)))
			)
		)
	)

;Question3
;This function is to mixes the element of X and Y into a single list
;This function will take elements from Y and X alternatively.
;If both X and Y are nil, then stop recursion
;If one of X and Y is nil, then return the other list.
;Testcases
; >(mix '(d e f) '(a b c))
; (a d b e c f)

; >(mix '(a) '(1 2 3))
; (1 a 2 3)

; >(mix '(d e f g h) '((a) (b c))) 
; ((a) d (b c) e f g h)

; >(mix nil '(1 2 3))
; (1 2 3)

; >(mix '(nil) '(1 2 3))
; (1 nil 2 3)
(defun mix(X Y)
	(cond
		((and (equal X nil) (equal Y nil))
			nil
			)
		((equal X nil)
			Y
			)
		((equal Y nil)
			X
			)
		((cons (car Y) (append (cons (car X) (append (mix (cdr X) (cdr Y)))))))

		)
	)

;Question4
;This function splits the list X into two sublist(L1 L2), by putting element into L1 and L2 alternatively
;for L1, take the first element of list X and append to the first element of split list X without the first two element
;for L2, take the second element of list X and append to the first elemnt of split list X without the first three element
; Testcases
; >(split '(1 2 3 4 5 6))
; ((1 3 5) (2 4 6))

; >(split '((a) (b c) (d e f) g h)) 
; (((a) (d e f) h) ((b c) g))

; >(split '())
; (nil nil)
(defun split(X)
	(if (equal X NIL)
		NIL
		(cons (cons (car X) (append (car (split (cddr X))))) (list (cons (cadr X) (append (car (split (cdddr X)))))))
		)
	)

;Question5
;5.1
;No, it is not always true (split (mix L2 L1)) returns the list (L1 L2). If length of L1 > length of L2 + 1, 
;then (split (mix L2 L1)) will not return (L1 L2)
;for example (split (mix '(1 2 3) '(7))) => ((7 2) (1 3))

; 5.2
; Yes, it is always true that (mix (cadr (split L)) (car (split L))) returns L

; (split L) will separate L into L1 and L2, and putting element of L into L1 and L2 alternatively
; (split L => (L1 L2)
; (cadr (L1 L2)) will extract L2
; (car (L1 L2)) will extract L1
; (mix L2 L1) will mix the element from L1, L2 to generate list L
; Since L1 and L2 are generate by (split L), thus length of L1 = length of L2 or length of L1 - 1 = length of L2
; so when we mix L1 and L2 toegther, we can generate the original list
; thus the statement is true

;ex (mix (cadr (split '(1 2))) (car (split '(1 2))))
; split '(1 2) => ((1) (2))
; (mix (cadr '((1) (2)) (car '(1) (2))))
; (mix (2) (1)) => (1 2)

;Question 6
; the function subsetsum will take two input: a given sum and a list of positive integers
; The function will return 
; 1.a list if a subset of positive integers add up to the given sum
; 2.nil if there wasn't a subset that will add up to the given sum
; Testcases
; >(subsetsum 5 '(1 2 3))
; (2 3)

; >(subsetsum 2 '(1 5 3))
; nil

; >(subsetsum 29 '(1 16 2 8 4))
; (1 16 8 4)

; >(subsetsum 10 '(1 1 5 6 8))
; (1 1 8)

; >(subsetsum 5 '(1 10 100 1000 10000))
; nil
(defun subsetsum (S L)
	(let ((x (sum L)))
		(if (= S x);If sum of list is equal to the given sum, return the list
			L
			(if (< S x);If the sum of list is larger than the given sum, find a subset 
				(findSubSetSum S nil L)
				NIL;The sum of list is small than the given sum, return nil
				);end if
			);end if
		);end let
	);end defun

;Helper function for subsetsum
; The function will take three parameter: the given sum, subList1, subList2
; subList1 and subList2 are subsets of the given list.
; First check the sum of sublist not including the first element will equal the given sum. 
; If no, then add the first element and see whether the sum of the list equal to the given sum 

(defun findSubSetSum(S subL1 subL2)
	(if (equal subL2 nil)
		(if (= S 0) subL1);End If
		(cond
			((= S 0);Found a match, return 
				subL1
				)
			((< S 0);The subsetsum is already greater than the sum, no need to tontinue
				nil
				)
			((> S (sum subL2));The remaining subset had a sum less than the given sum, no need to contine
				nil
				)
			((let ((x (findSubSetSum S subL1 (cdr subL2))));Not include the first element
				(if (equal x nil)	;If not match is found, include the first element and check
					(let ((y (findSubSetSum (- S (car subL2)) (append subL1 (list (car subL2))) (cdr subL2))));Include the first element
						(if (not (null y))	;Found a match
							Y 
							);End if
						);End let y
					x 	;Found a match
					);End If
				);End Let x
				);End first cond
			);End cond
		);End If
	);End Defun

;helper function for question6 subsetsum
;Input: a list of numbers
;Will add the list of numbers recursively and return the total sum
(defun sum(L)
	(if (not (null L))
		(+ (first L) (sum (rest L)))
		0
		)
	)











