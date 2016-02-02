;;;
;;;
;;;
(define-module lang.c.c89-gram (extend lang.core)
  (use lalr)
  (use lang.c.c89-scan)
  (export c89-gram))
(select-module lang.c.c89-gram)

(define c89-gram
  (lalr-parser
   (expect: 1)  ; IF-ELSE
   (output: c89-gram "c89-gram.yy.scm")
   (out-table: "c89-gram.out")
   (ID
    SEMICOLON COMMA
    ;; LCBRA={  RCBRA=} LSBRA=[  RSBRA=]
    LCBRA RCBRA LSBRA RSBRA
    ;; LPAREN=( RPAREN=) OR=| DOT=. COLON=:
    LPAREN RPAREN OR DOT COLON

    ~ ! + - * / ^ & % = ? < >

    IDENTIFIER STRING
    INTEGER-CONSTANT CHARACTER-CONSTANT
    FLOAT-CONSTANT DOUBLE-CONSTANT LONG-DOUBLE-CONSTANT

    SIZEOF
    PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
    MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN
    LEFT_ASSIGN RIGHT_ASSIGN
    AND_ASSIGN XOR_ASSIGN OR_ASSIGN

    TYPEDEF TYPEDEF-NAME
    EXTERN STATIC AUTO REGISTER
    CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
    STRUCT UNION ENUM ELLIPSIS RANGE
    CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
    __BUILTIN_VA_LIST
    )

   (program
    ()
    (file)
    )

   (file
    (external_definition)
    (file external_definition)
    )

   (external_definition
    (function_definition)     : (compile $1)
    (declaration)             : (compile $1)
    )

   (function_definition
    (declarator function_body)                         : (list 'function-definition $1 #f $2)
    (declaration_specifiers declarator function_body)  : (list 'function-definition $2 $1 $3)
    )

   (function_body
    (compound_statement)                               : (list 'function-body #f $1)
    (declaration_list compound_statement)              : (list 'function-body $1 $2)
    )

   (declaration
    (declaration_specifiers SEMICOLON)                       : (list 'declaration $1 #f)
    (declaration_specifiers init_declarator_list SEMICOLON)  : (list 'declaration $1 $2)
    )

   (declaration_specifiers
    (storage_class_specifier)                            : (list $1)
    (storage_class_specifier declaration_specifiers)     : (cons $1 $2)
    (type_specifier)                                     : (list $1)
    (type_specifier declaration_specifiers)              : (cons $1 $2)
    )

   (init_declarator_list
    (init_declarator)                                    : (list $1)
    (init_declarator_list COMMA init_declarator)         : (append $1 (list $2))
    )

   (init_declarator
    (declarator)                                         : (list 'init-declarator $1 #f)
    (declarator = initializer)                           : (list 'init-declarator $1 $3)
    )

   (storage_class_specifier
    (TYPEDEF)                   : (list 'storage-class-specifier $1)
    (EXTERN)                    : (list 'storage-class-specifier $1)
    (STATIC)                    : (list 'storage-class-specifier $1)
    (AUTO)                      : (list 'storage-class-specifier $1)
    (REGISTER)                  : (list 'storage-class-specifier $1)
    )

   (type_specifier
    (CHAR)                         : (list 'type-specifier $1)
    (SHORT)                        : (list 'type-specifier $1)
    (INT)                          : (list 'type-specifier $1)
    (LONG)                         : (list 'type-specifier $1)
    (SIGNED)                       : (list 'type-specifier $1)
    (UNSIGNED)                     : (list 'type-specifier $1)
    (FLOAT)                        : (list 'type-specifier $1)
    (DOUBLE)                       : (list 'type-specifier $1)
    (CONST)                        : (list 'type-specifier $1)
    (VOLATILE)                     : (list 'type-specifier $1)
    (VOID)                         : (list 'type-specifier $1)
    (struct_or_union_specifier)    : (list 'type-specifier $1)
    (enum_specifier)               : (list 'type-specifier $1)
    (TYPEDEF-NAME)                 : (list 'type-specifier $1)
    (__BUILTIN_VA_LIST)            : (list 'type-specifier $1)
    )

   (primary_expr
    (IDENTIFIER)                   : $1
    (constant)                     : $1
    (STRING)                       : $1
    (LPAREN expr RPAREN)           : $2
    )

   (postfix_expr
    (primary_expr)                                   : $1
    (postfix_expr LSBRA expr RSBRA)                  : (list 'ARRAY-REF  $1 $3)
    (postfix_expr LPAREN RPAREN)                     : (list 'FUNCALL   $1 '())
    (postfix_expr LPAREN argument_expr_list RPAREN)  : (list 'FUNCALL   $1 $3)
    (postfix_expr DOT IDENTIFIER)                    : (list 'STRUCT-REF $1 $3)
    (postfix_expr PTR_OP IDENTIFIER)                 : (list 'STRUCT-PTR-REF $1 $3)
    (postfix_expr INC_OP)                            : (list 'POST-INCREMENT $1)
    (postfix_expr DEC_OP)                            : (list 'POST-DECREMENT $1)
    )

   (argument_expr_list
    (assignment_expr)                                : (list $1)
    (argument_expr_list COMMA assignment_expr)       : (append $1 (list $3))
    )

   (unary_expr
    (postfix_expr)                     : $1
    (INC_OP unary_expr)                : (list 'PRE-INCREMENT $2)
    (DEC_OP unary_expr)                : (list 'PRE-DECREMENT $2)
    (unary_operator cast_expr)         : (list $1 $2)
    (SIZEOF unary_expr)                : (list 'SIZEOF $2)
    (SIZEOF LPAREN type_name RPAREN)   : (list 'SIZEOF $3)
    )

   (unary_operator
    (&)
    (*)
    (+)
    (-)
    (~)
    (!)
    )

   (cast_expr
    (unary_expr)                            : $1
    (LPAREN type_name RPAREN cast_expr)     : (list 'cast $2 $4)
    )

   (multiplicative_expr
    (cast_expr)                              : $1
    (multiplicative_expr * cast_expr)        : (list '* $1 $3)
    (multiplicative_expr / cast_expr)        : (list '/ $1 $3)
    (multiplicative_expr % cast_expr)        : (list '% $1 $3)
    )

   (additive_expr
    (multiplicative_expr)                     : $1
    (additive_expr + multiplicative_expr)     : (list '+ $1 $3)
    (additive_expr - multiplicative_expr)     : (list '- $1 $3)
    )

   (shift_expr
    (additive_expr)                            : $1
    (shift_expr LEFT_OP additive_expr)         : (list 'LEFT_OP $1 $3)
    (shift_expr RIGHT_OP additive_expr)        : (list 'RIGHT_OP $1 $3)
    )

   (relational_expr
    (shift_expr)                                : $1
    (relational_expr < shift_expr)              : (list '< $1 $3)
    (relational_expr > shift_expr)              : (list '> $1 $3)
    (relational_expr LE_OP shift_expr)          : (list 'LE_OP $1 $3)
    (relational_expr GE_OP shift_expr)          : (list 'GE_OP $1 $3)
    )

   (equality_expr
    (relational_expr)                            : $1
    (equality_expr EQ_OP relational_expr)        : (list 'EQ_OP $1 $3)
    (equality_expr NE_OP relational_expr)        : (list 'EQ_OP $1 $3)
    )

   (and_expr
    (equality_expr)                              : $1
    (and_expr & equality_expr)                   : (list '^ $1 $3)
    )


   (exclusive_or_expr
    (and_expr)                                   : $1
    (exclusive_or_expr ^ and_expr)               : (list '^ $1 $3)
    )


   (inclusive_or_expr
    (exclusive_or_expr)                           : $1
    (inclusive_or_expr OR exclusive_or_expr)      : (list 'OR $1 $3)
    )


   (logical_and_expr
    (inclusive_or_expr)                          : $1
    (logical_and_expr AND_OP inclusive_or_expr)  : (list 'AND_OP $1 $3) 
    )

   (logical_or_expr
    (logical_and_expr)                         : $1
    (logical_or_expr OR_OP logical_and_expr)   : (list 'OR_OP $1 $3)
    )

   (conditional_expr
    (logical_or_expr)                                           : $1
    (logical_or_expr ? logical_or_expr COLON conditional_expr)  : (list '? $1 $3 $5)
    )

   (assignment_expr
    (conditional_expr)                                : $1
    (unary_expr assignment_operator assignment_expr)  : (list $1 $2 $3)
    )


   (assignment_operator
    (=)
    (MUL_ASSIGN)
    (DIV_ASSIGN)
    (MOD_ASSIGN)
    (ADD_ASSIGN)
    (SUB_ASSIGN)
    (LEFT_ASSIGN)
    (RIGHT_ASSIGN)
    (AND_ASSIGN)
    (XOR_ASSIGN)
    (OR_ASSIGN)
    )

   (expr
    (assignment_expr)                : (list $1)
    (expr COMMA assignment_expr)     : (append $1 (list $1))
    )


   (constant_expr
    (conditional_expr)
    )

   (struct_or_union_specifier
    (struct_or_union IDENTIFIER LCBRA struct_declaration_list RCBRA)
    (struct_or_union LCBRA struct_declaration_list RCBRA)
    (struct_or_union IDENTIFIER)
    )

   (struct_or_union
    (STRUCT)
    (UNION)
    )

   (struct_declaration_list
    (struct_declaration)
    (struct_declaration_list struct_declaration)
    )

   (struct_declaration
    (type_specifier_list struct_declarator_list SEMICOLON)
    )

   (struct_declarator_list
    (struct_declarator)
    (struct_declarator_list COMMA struct_declarator)
    )

   (struct_declarator
    (declarator)
    (COLON constant_expr)
    (declarator COLON constant_expr)
    )

   (enum_specifier
    (ENUM LCBRA enumerator_list RCBRA)
    (ENUM IDENTIFIER LCBRA enumerator_list RCBRA)
    (ENUM IDENTIFIER)
    )

   (enumerator_list
    (enumerator)
    (enumerator_list COMMA enumerator)
    )

   (enumerator
    (IDENTIFIER)
    (IDENTIFIER = constant_expr)
    )

   (declarator
    (declarator2)                 : (list 'declarator $1 #f)
    (pointer declarator2)         : (list 'declarator $2 $1)
    )

   (declarator2
    (IDENTIFIER)                  : $1
    (LPAREN declarator RPAREN)               : (list 'cast $1)
    (declarator2 LSBRA RSBRA)                : (list 'array $1 #f)
    (declarator2 LSBRA constant_expr RSBRA)  : (list 'array $1 $3)
    (declarator2 LPAREN RPAREN)                           : (list 'function $1 #f)
    (declarator2 LPAREN parameter_type_list RPAREN)       : (list 'function $1 $3)
    (declarator2 LPAREN parameter_identifier_list RPAREN) : (list 'function $1 $3)
    )

   (pointer
    (*)
    (* type_specifier_list)
    (* pointer)
    (* type_specifier_list pointer)
    )

   (type_specifier_list
    (type_specifier)                           : (list $1)
    (type_specifier_list type_specifier)       : (append $1 (list $2))
    )

   (parameter_identifier_list
    (identifier_list)
    (identifier_list COMMA ELLIPSIS)
    )

   (identifier_list
    (IDENTIFIER)
    (identifier_list COMMA IDENTIFIER)
    )

   (parameter_type_list
    (parameter_list)                     : (list 'parameter-type-list $1 #f)
    (parameter_list COMMA ELLIPSIS)      : (list 'parameter-type-list $1 #t)
    )

   (parameter_list
    (parameter_declaration)                      : (list $1)
    (parameter_list COMMA parameter_declaration) : (append $1 (list $3))
    )

   (parameter_declaration
    (type_specifier_list declarator)     : (list $1 $2)
    (type_name)                          : $1
    )

   (type_name
    (type_specifier_list)                       : (list 'type-name $1 #f)
    (type_specifier_list abstract_declarator)   : (list 'type-name $1 $2)
    )

   (abstract_declarator
    (pointer)
    (abstract_declarator2)
    (pointer abstract_declarator2)
    )


   (abstract_declarator2
    (LPAREN abstract_declarator RPAREN)
    (LSBRA RSBRA)
    (LSBRA constant_expr RSBRA)
    (abstract_declarator2 LSBRA RSBRA)
    (abstract_declarator2 LSBRA constant_expr RSBRA)
    (LPAREN RPAREN)
    (LPAREN parameter_type_list RPAREN)
    (abstract_declarator2 LPAREN RPAREN)
    (abstract_declarator2 LPAREN parameter_type_list RPAREN)
    )

   (initializer
    (assignment_expr)                     : $1
    (LCBRA initializer_list RCBRA)        : $2
    (LCBRA initializer_list COMMA RCBRA)  : $2
    )

   (initializer_list
    (initializer)                         : (list $1)
    (initializer_list COMMA initializer)  : (append $1 (list $1))
    )

   (statement
    (labeled_statement)                : $1
    (compound_statement)               : $1
    (expression_statement)             : $1
    (selection_statement)              : $1
    (iteration_statement)              : $1
    (jump_statement)                   : $1
    )

   (labeled_statement
    (IDENTIFIER COLON statement)              : (list 'SET-LABEL $1 $3)
    (CASE constant_expr COLON statement)      : (list 'CASE $2 $4) 
    (DEFAULT COLON statement)                 : (list 'DEFAULT $3)
    )

   (compound_statement
    (LCBRA RCBRA)                                 : (list 'BLOCK #f '((NOP)))
    (LCBRA statement_list RCBRA)                  : (list 'BLOCK #f $2)
    (LCBRA declaration_list RCBRA)                : (list 'BLOCK $2 #f)
    (LCBRA declaration_list statement_list RCBRA) : (list 'BLOCK $2 $3)
    (error RCBRA)
    )

   (declaration_list
    (declaration)                    : (list $1)
    (declaration_list declaration)   : (append $1 (list $2))
    )

   (statement_list
    (statement)                      : (list $1)
    (statement_list statement)       : (append $1 (list $2))
    )

   (expression_statement
    (SEMICOLON)                      : '(NOP)
    (expr SEMICOLON)                 : $1
    (error SEMICOLON)
    )

   (selection_statement
    (IF LPAREN expr RPAREN statement)                  : (list 'IF $3 $5 #f)
    (IF LPAREN expr RPAREN statement ELSE statement)   : (list 'IF $3 $5 $7)
    (SWITCH LPAREN expr RPAREN statement)              : (list 'SWITCH $3 $5)
    )

   (iteration_statement
    (WHILE LPAREN expr RPAREN statement)                             : (list 'WHILE $3 $5)
    (DO statement WHILE LPAREN expr RPAREN SEMICOLON)                : (list 'DO $2 $5)
    (FOR LPAREN SEMICOLON SEMICOLON RPAREN statement)                : (list 'FOR #f #f #f $6)
    (FOR LPAREN SEMICOLON SEMICOLON expr RPAREN statement)           : (list 'FOR #f #f $5 $7)
    (FOR LPAREN SEMICOLON expr SEMICOLON RPAREN statement)           : (list 'FOR #f $4 #f $7)
    (FOR LPAREN SEMICOLON expr SEMICOLON expr RPAREN statement)      : (list 'FOR #f $4 $6 $8)
    (FOR LPAREN expr SEMICOLON SEMICOLON RPAREN statement)           : (list 'FOR $3 #f #f $7)
    (FOR LPAREN expr SEMICOLON SEMICOLON expr RPAREN statement)      : (list 'FOR $3 #f $6 $8)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON RPAREN statement)      : (list 'FOR $3 $5 #f $8)
    (FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN statement) : (list 'FOR $3 $5 $7 $9)
    )

   (jump_statement
    (GOTO IDENTIFIER SEMICOLON)  : (list 'GOTO $2)
    (CONTINUE SEMICOLON)         : (list 'CONTINUE)
    (BREAK SEMICOLON)            : (list 'BREAK)
    (RETURN SEMICOLON)           : (list 'RETURN #f)
    (RETURN expr SEMICOLON)      : (list 'RETURN $2)
    )

   (constant
    (INTEGER-CONSTANT)     : $1
    (CHARACTER-CONSTANT)   : $1
    (FLOAT-CONSTANT)       : $1
    (DOUBLE-CONSTANT)      : $1
    (LONG-DOUBLE-CONSTANT) : $1
    )
   ))

(define (pppp v)
  (define (ff v n)
    (let ((sp (make-string n #\space)))
      (define (wri x) (display sp) (write x) (newline))
      (define (dsp x) (display sp) (display x) (newline))
      (for-each (lambda (x)
                  (if (pair? x)
                      (begin
                        (dsp "(")
                        (ff x (+ n 4))
                        (dsp ")"))
                      (wri x)))
                v)))
  (newline)
  (display "(")(newline)
  (ff v 1)
  (display ")")(newline)
  )

(define (ppvec v)
  (define (ff v n)
    (let ((sp (make-string n #\|)))
      (define (wri x) (display sp) (write x) (newline))
      (vector-for-each (lambda (x)
                  (if (vector? x)
                      (ff x (+ n 1))
                      (wri x)))
                v)))
  (newline)
  (ff v 1))

(define (compile e)
  (cond ((null? e) '())
        (else
         (case (car e)
           ((declaration) (do-decl (cadr e) (caddr e)))
           (else '()))))
  (pppp e))

(define (do-decl declspecs init-list)
  (let ((x (assoc-ref declspecs 'storage-class-specifier #f eq?)))
    (print "xxx: " x)
    (when (and x (eq? (caar x) 'TYPEDEF))
      (let* ((y (cadar init-list))
             (z (cadadr y)))
        (print "yyy: " y)
        (print "zzz: " z)
        (register-typedef-for-c89-scan (string->symbol z))))))

(provide "lang/c/c89-gram")
