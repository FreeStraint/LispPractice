;collaborator Mingjun Zhao

(defun fl-interp (E P)
	(cond
		((atom E) E)
		((let ((f (car E)) (arg (cdr E)) (fun (getFunction (car E) P)))
			(cond
				;handle built-in functions
				((equal f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((equal f 'atom) (atom (car (nestOper (car arg) P))))
				((equal f 'first) (apply f (nest arg)))
				((equal f 'rest) (apply f (nest arg)))
				((equal f 'cons) (apply f (nest arg)))
				((equal f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((equal f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
				((equal f 'isnumber) (numberp (fl-interp (car arg) P)))
				((equal f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
				((xmember f) (apply f (nestOper arg P)))
				
				;User define function
				((not (null P))
					(let ((var (getVariables (cdr fun))) (body (getFUnctionBody fun))) 
						;After var is replaced by arg, we can simply interpret the statement with only the primative functions
						(fl-interp (car (substitutes var body arg)) P)						
						)
					)
				(E)
				);end cond
			);End let
		);End sub cond
		);End cond
	);End defun


;Idea from beta reduction and a friend
;Where I substitute the first variable to the first argument 
(defun substitutes (var body arg)
	(if (null var)
		body
	;Only one variable
		(if (null (cdr var))
			(switch (car var) body (car arg))
			(substitutes (cdr var) (switch (car var) body (car arg)) (cdr arg))
			)
		)
	)
;A sub function for substitutes
;Replace var with arg in the function body
(defun switch (var body arg)
	(if (equal var nil)
		nil
		(if (atom body)
			(if (equal var body)
				arg
				body
				)
			(cons (switch var (car body) arg) (switch var (cdr body) arg))
			)
		)
	)

;Get the user define function that match f
(defun getFunction (f P)
	(if (null P)
		nil
		(IF (equal f (caar P))
			(car P)
			(getFunction f (cdr P))
			)
		)
	)

;Get variables
;The variables between the function name and '=
(defun getVariables(P)
	(if (equal (car P) '=)
		nil
		(cons (car P) (getVariables (cdr P)))
		);End if
	);End defun

;Get function body
;The function body is the porition to the right of the '= 
(defun getFUnctionBody(P)
	(if (equal (car P) '=)
		(cdr P)
		(getFUnctionBody (cdr P))
		);End if
	);End defun

;Check whether the input function is Primitives
(defun xmember (f)
	(cond
		((equal f '+) T)
		((equal f '-) T)
		((equal f '<) T)
		((equal f '>) T)
		((equal f '=) T)
		((equal f '*) T)
		((equal f 'not) T)
		((equal f 'null) T)
		((equal f 'eq) T)
		((equal f '+) T)
		)
	)

;Exact the number from deeper level
(defun nestOper (E P)
	(if (null E)
		nil
		(cons (fl-interp (car E) P) (nestOper (cdr E) P))
		);End if
	);End defun

;Extract the number from deeper level
;Only for first, rest and cons
(defun nest(E)
	(if (null E)
		nil
		(cons (fl-interp (car E) nil) (nest (cdr E)))
		)
	)
