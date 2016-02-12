;;;
;;;     C89 with some extensions and a few bugs.
;;;
;;;     BUGS
;;;      1. Type redefinition (doing typedef for the same
;;;         type-name more than once) does not work correctly.
;;;
;;;
(define-module lang.c.c89-gram (extend lang.core)
  (use gauche.parameter)
  (use lang.lalr.lalr)    ;; This should eventually become lang.lalr .
  (use lang.c.c89-scan)
  (export make-c89-parse))
(select-module lang.c.c89-gram)

(define (make-c89-parse :optional (compile compile) (define-type define-type))
  (lalr-parser
   (expect: 2)  ; ELSE, DOUBLE
   ;;(output: c89-gram "c89-gram.yy.scm")
   ;;(out-table: "c89-gram.out")
   ;;
   ;;  C89 grammar with some extensions towards C99.
   ;;
   ;;  Based on usenet/net.sources/ansi.c.grammar.Z
   ;;
   ;;    From: tps@sdchem.UUCP (Tom Stockfisch)
   ;;    Newsgroups: net.sources
   ;;    Subject: ANSI C draft yacc grammar
   ;;    Message-ID: <645@sdchema.sdchem.UUCP>
   ;;    Date: 3 Mar 87 21:31:17 GMT
   ;;    References: <403@ubc-vision.UUCP>
   ;;    Sender: news@sdchem.UUCP
   ;;    Reply-To: tps@sdchemf.UUCP (Tom Stockfisch)
   ;;    Organnization: UC San Diego
   ;;    Lines: 775
   ;;
   ;;  Updates can be found at:
   ;;
   ;;    http://www.quut.com/c/ANSI-C-grammar-y-2011.html
   ;;    http://www.quut.com/c/ANSI-C-grammar-y-1999.html
   ;;
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

    TYPEDEF TYPE_NAME
    EXTERN STATIC AUTO REGISTER
    VOID CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE
    CONST VOLATILE
    INLINE NORETURN
    STRUCT UNION ENUM ELLIPSIS RANGE
    CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

    ASM ALIGNOF VA_LIST VA_ARG
    )

   (program
    ()
    (file)                       : $1
    )

    (file
    (external_declaration)       : (list $1)
    (file external_declaration)  : (append $1 $2)
    )

   (external_declaration
    (function_definition)            : (compile $1)
    (function_definition SEMICOLON)  : (compile $1)
    (type_definition)                : (compile $1)
    (declaration)                    : (compile $1)
    )

   (function_definition
    (declaration_specifiers declarator declaration_list compound_statement) : (list 'DEFINE-FUNCTION $2 $1 $3 $4)
    (declaration_specifiers declarator compound_statement) : (list 'DEFINE-FUNCTION $2 $1 'w/o-declaration-list $3)
    (declarator declaration_list compound_statement) : (list 'DEFINE-FUNCTION $1 'w/o-declaration-specifiers $2 $3)
    (declarator compound_statement) : (list 'DEFINE-FUNCTION $1 'w/o-declaration-list 'w/o-declaration-specifiers $2)
    )

   (type_definition
    (TYPEDEF declaration_specifiers typedef_declarator_list SEMICOLON) :  (begin (define-type $3 $2)
                                                                                 (list 'DEFINE-TYPE $3 $2))
    )

   #;(identifier
    (IDENTIFIER)  : $1
    ;; (TYPE_NAME : $1
    )

   (primary_expr
    (IDENTIFIER)                   : $1
    (constant)                     : $1
    (string_list)                  : $1
    (LPAREN expr RPAREN)           : $2
    )

   (string_list
    (STRING)                       : (list 'STRING-LIST $1)
    (string_list STRING)           : (append $1 (list $2))
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
    (compound_literal)                               : $1
    (LPAREN compound_statement RPAREN)               : $2      ; ???
    )

   (compound_literal
    (LPAREN type_name RPAREN LCBRA initializer_list RCBRA)        : (list 'COMPOUND-LITERAL $2 $5)
    (LPAREN type_name RPAREN LCBRA initializer_list COMMA RCBRA)  : (list 'COMPOUND-LITERAL $2 $5)
    )

   (argument_expr_list
    (assignment_expr)                              : (list $1)
    (argument_expr_list COMMA assignment_expr)     : (append $1 (list $3))
    ;;(assignment_expr COMMA argument_expr_list)   : (cons $1 (list $3))
    )

   (unary_expr
    (postfix_expr)                     : $1
    (INC_OP unary_expr)                : (list 'PRE-INCREMENT $2)
    (DEC_OP unary_expr)                : (list 'PRE-DECREMENT $2)
    (unary_operator cast_expr)         : (list $1 $2)
    (SIZEOF unary_expr)                : (list 'SIZEOF $2)
    (SIZEOF LPAREN type_name RPAREN)   : (list 'SIZEOF $3)
    (ALIGNOF unary_expr)               : (list 'ALIGNOF $2)
    (ALIGNOF LPAREN type_name RPAREN)  : (list 'ALIGNOF $3)
    (VA_ARG LPAREN IDENTIFIER COMMA type_name RPAREN) : (list 'VA_ARG $3 $5)
    )

   (unary_operator
    (&)                   : 'unary-&
    (*)                   : 'unary-*
    (+)                   : 'unary-+
    (-)                   : 'unary--
    (~)                   : 'unary-~
    (!)                   : 'unary-!
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
    (multiplicative_expr)                    : $1
    (additive_expr + multiplicative_expr)    : (list '+ $1 $3)
    (additive_expr - multiplicative_expr)    : (list '- $1 $3)
    )

   (shift_expr
    (additive_expr)                          : $1
    (shift_expr LEFT_OP additive_expr)       : (list 'LEFT_OP $1 $3)
    (shift_expr RIGHT_OP additive_expr)      : (list 'RIGHT_OP $1 $3)
    )

   (relational_expr
    (shift_expr)                             : $1
    (relational_expr < shift_expr)           : (list '< $1 $3)
    (relational_expr > shift_expr)           : (list '> $1 $3)
    (relational_expr LE_OP shift_expr)       : (list 'LE_OP $1 $3)
    (relational_expr GE_OP shift_expr)       : (list 'GE_OP $1 $3)
    )

   (equality_expr
    (relational_expr)                        : $1
    (equality_expr EQ_OP relational_expr)    : (list 'EQ_OP $1 $3)
    (equality_expr NE_OP relational_expr)    : (list 'EQ_OP $1 $3)
    )

   (and_expr
    (equality_expr)                          : $1
    (and_expr & equality_expr)               : (list '& $1 $3)
    )

   (exclusive_or_expr
    (and_expr)                               : $1
    (exclusive_or_expr ^ and_expr)           : (list '^ $1 $3)
    )

   (inclusive_or_expr
    (exclusive_or_expr)                      : $1
    (inclusive_or_expr OR exclusive_or_expr) : (list 'OR $1 $3)
    )

   (logical_and_expr
    (inclusive_or_expr)                         : $1
    (logical_and_expr AND_OP inclusive_or_expr) : (list 'AND_OP $1 $3)
    )

   (logical_or_expr
    (logical_and_expr)                       : $1
    (logical_or_expr OR_OP logical_and_expr) : (list 'OR_OP $1 $3)
    )

   (conditional_expr
    (logical_or_expr)                                          : $1
    (logical_or_expr ? logical_or_expr COLON conditional_expr) : (list '? $1 $3 $5)
    )

   (assignment_expr
    (conditional_expr)                                : $1
    (unary_expr assignment_operator assignment_expr)  : (list $2 $1 $3)
    )

   (assignment_operator
    (=)                    : 'ASSIGN
    (MUL_ASSIGN)           : 'MUL_ASSIGN
    (DIV_ASSIGN)           : 'DIV_ASSIGN
    (MOD_ASSIGN)           : 'MOD_ASSIGN
    (ADD_ASSIGN)           : 'ADD_ASSIGN
    (SUB_ASSIGN)           : 'SUB_ASSIGN
    (LEFT_ASSIGN)          : 'LEFT_ASSIGN
    (RIGHT_ASSIGN)         : 'RIGHT_ASSIGN
    (AND_ASSIGN)           : 'AND_ASSIGN
    (XOR_ASSIGN)           : 'XOR_ASSIGN
    (OR_ASSIGN)            : 'OR_ASSIGN
    )

   (expr
    (assignment_expr)             : $1
    (expr COMMA assignment_expr)  : (append $1 $3)
    )

   (constant_expr
    (conditional_expr)            : $1
    )

   (declaration
    (declaration_specifiers SEMICOLON)                                 : (list 'DECLARATION 'w/o-init-declarator-list $1)
    (declaration_specifiers init_declarator_list SEMICOLON)            : (list 'DECLARATION $2 $1)
    (declaration_specifiers init_declarator_list asm_label SEMICOLON)  : (list 'DECLARATION $2 $1) ; ignore asm label
    )

   (asm_label
    (ASM LPAREN RPAREN)
    (ASM LPAREN string_list RPAREN)
    )

   (declaration_specifiers
    (type_specifier)                                     : (list 'w/o-storage-class-specifier $1 #f #f)
    (function_specifier)                                 : (list 'w/o-storage-class-specifier $1 #f #f)
    (type_qualifier)                                     : (list 'w/o-storage-class-specifier (list 'INT 'INT 'SIGNED) $1 #f)

    (type_qualifier type_specifier)                      : (list 'w/o-storage-class-specifier $2 $1 #f)
    (type_specifier type_qualifier)                      : (list 'w/o-storage-class-specifier $2 #f $1)
    (function_specifier type_specifier)                  : (list 'w/o-storage-class-specifier $2 $1 #f)
    (type_specifier function_specifier)                  : (list 'w/o-storage-class-specifier $1 $2 #f)

    (storage_class_specifier)                                    : (list $1 (list 'INT 'INT 'SIGNED) #f #f)
    (storage_class_specifier type_specifier)                     : (list $1 $2 #f #f)
    (storage_class_specifier function_specifier)                 : (list $1 (list 'INT 'INT 'SIGNED) $2 #f)
    (storage_class_specifier function_specifier type_specifier)  : (list $1 $3 $2 #f)
    (storage_class_specifier type_specifier function_specifier)  : (list $1 $2 $3 #f)

    (storage_class_specifier type_qualifier)                     : (list $1 (list 'INT 'INT 'SIGNED) $2 #f)
    (storage_class_specifier type_qualifier type_specifier)      : (list $1 $3 $2 #f)
    (storage_class_specifier type_specifier type_qualifier)      : (list $1 $3 #f $2)
     (type_qualifier storage_class_specifier type_specifier)     : (list $2 $3 $1 #f)
    )

   (float_type_specifier
    (FLOAT)                                   : (list 'FLOAT 'SINGLE)
    (DOUBLE)                                  : (list 'FLOAT 'DOUBLE)
    (LONG DOUBLE)                             : (list 'FLOAT 'LONG)
    )

   (int_type_name
    (CHAR)                                    : 'CHAR
    (INT)                                     : 'INT
    (SHORT)                                   : 'SHORT
    (LONG)                                    : 'LONG
    (SIGNED)                                  : 'SIGNED
    (UNSIGNED)                                : 'UNSIGNED
    )

   (int_type_specifier
    (int_type_name)                           : (list $1)
    (int_type_name int_type_specifier)        : (cons $1 $2)
    )

   (typedef_declarator_list
    (typedef_declarator)                                 : (list $1)
    (typedef_declarator_list COMMA typedef_declarator)   : (append $1 (list $3))
    )

   (init_declarator
    (declarator)                                         : (list $1 :init #f)
    (declarator = initializer)                           : (list $1 :init $3)
    )

   (init_declarator_list
    (init_declarator)                                    : (list $1)
    (init_declarator_list COMMA init_declarator)         : (append $1 (list $3))
    )

   (storage_class_specifier
    (EXTERN)                       :  'EXTERN
    (STATIC)                       :  'STATIC
    (AUTO)                         :  'AUTO
    (REGISTER)                     :  'REGISTER
    )

   (type_specifier
    (VOID)                         :  (list 'VOID)
    (int_type_specifier)           :  $1
    (float_type_specifier)         :  $1
    (struct_or_union_specifier)    :  $1
    (enum_specifier)               :  $1
    (TYPE_NAME)                    :  $1
    (VA_LIST)                      :  (list 'VA_LIST)
    )

   (struct_or_union_specifier
    (struct_or_union IDENTIFIER LCBRA struct_declaration_list RCBRA) : (list $1 $2 $4)
    (struct_or_union TYPE_NAME  LCBRA struct_declaration_list RCBRA) : (list $1 $2 $4)   ;; ???
    (struct_or_union LCBRA RCBRA)                                    : (list $1 #f 'empty-struct-declaration-list)
    (struct_or_union LCBRA struct_declaration_list RCBRA)            : (list $1 #f $3)
    (struct_or_union IDENTIFIER)                                     : (list $1 $2 'w/o-struct-declaration-list)
    (struct_or_union IDENTIFIER LCBRA RCBRA)                         : (list $1 $2 'empty-struct-declaration-list)
    (struct_or_union TYPE_NAME)                                      : (list $1 $2 'w/o-struct-declaration-list)
    )

   (struct_or_union
    (STRUCT)                       : 'STRUCT
    (UNION)                        : 'UNION
    )

   (struct_declaration_list
    (struct_declaration)                          : (list $1)
    (struct_declaration_list struct_declaration)  : (append $1 (list $2))
    )

   (struct_declaration
    (specifier_qualifier_list struct_declarator_list SEMICOLON) : (list $2 $1)
    )

   (specifier_qualifier_list
    (type_specifier)                              : (list $1)
    (type_qualifier)                              : (list $1)
    (type_specifier specifier_qualifier_list)     : (cons $1 $2)
    (type_qualifier specifier_qualifier_list)     : (cons $1 $2)
    )

   (struct_declarator_list
    (struct_declarator)                              : $1
    (struct_declarator_list COMMA struct_declarator) : (append $1 $3)
    )

   (struct_declarator
    (declarator)                          : (list $1 'w/o-bitfield)
    (COLON constant_expr)                 : (list 'w/o-declarator $2)
    (declarator COLON constant_expr)      : (list $1 $3)
    )

   (enum_specifier
    (ENUM LCBRA enumerator_list RCBRA)                  : (list 'ENUM #f $3)
    (ENUM IDENTIFIER LCBRA enumerator_list RCBRA)       : (list 'ENUM $2 $4)
    (ENUM IDENTIFIER LCBRA enumerator_list COMMA RCBRA) : (list 'ENUM $2 $4)
    (ENUM IDENTIFIER)                                   : (list 'ENUM $2 #f)
    (ENUM TYPE_NAME  LCBRA enumerator_list RCBRA)       : (list 'ENUM $2 $4)
    (ENUM TYPE_NAME  LCBRA enumerator_list COMMA RCBRA) : (list 'ENUM $2 $4)
    (ENUM TYPE_NAME)                                    : (list 'ENUM $2 #f)
    )

   (enumerator_list
    (enumerator)                        : (list $1)
    (enumerator_list COMMA enumerator)  : (append $1 (list $3))
    )

   (enumerator
    (IDENTIFIER)                        : (list 'enumerator $1 'w/o-constant-expr)
    (IDENTIFIER = constant_expr)        : (list 'enumerator $1 $3)
    ;;(TYPE_NAME)                       : (list 'enumerator $1 'w/o-constant-expr)
    ;;(TYPE_NAME = constant_expr)       : (list 'enumerator $1 $3)
    )

   (type_qualifier
    (CONST)                       :  'CONST
    (VOLATILE)                    :  'VOLATILE
    )

   (function_specifier
    (INLINE)                      :  'INLINE
    (NORETURN)                    :  'NORETURN
    )

   (typedef_declarator
    (pointer typedef_declarator2) : (append $2 $1)
    (typedef_declarator2)         : (append $1 (list 'non-pointer))
    )

   (typedef_declarator2
    (IDENTIFIER)                                            : (list $1)
    (TYPE_NAME)                                             : (list $1)
    (LPAREN typedef_declarator RPAREN)                      : $2
    (typedef_declarator2 LSBRA assignment_expr RSBRA)       : (append $1 (list $3 'array))
    (typedef_declarator2 LSBRA RSBRA)                       : (append $1 (list #f 'array))
    (typedef_declarator2 LPAREN parameter_type_list RPAREN) : (append $1 (list $3 'function))
    (typedef_declarator2 LPAREN IDENTIFIER_list RPAREN)     : (append $1 (list $3 'function))
    (typedef_declarator2 LPAREN RPAREN)                     : (append $1 (list #f 'function))
    )

   (declarator
    (pointer declarator2)          : (append $2 $1)
    (declarator2)                  : (append $1 (list 'non-pointer))
    )

   (declarator2
    (IDENTIFIER)                                    : (list $1)
    (LPAREN declarator RPAREN)                      : $2
    (declarator2 LSBRA assignment_expr RSBRA)       : (append $1 (list $3 'array))
    (declarator2 LSBRA RSBRA)                       : (append $1 (list #f 'array))
    (declarator2 LPAREN parameter_type_list RPAREN) : (append $1 (list $3 'function))
    (declarator2 LPAREN IDENTIFIER_list RPAREN)     : (append $1 (list $3 'function))
    (declarator2 LPAREN RPAREN)                     : (append $1 (list #f 'function))
    )

   (pointer
    (*)                              : (list '*)
    (* type_qualifier_list)          : (cons '* $2)
    (* pointer)                      : (cons '* $2)
    (* type_qualifier_list pointer)  : (append (cons '* $2) $3)
    )

   (type_qualifier_list
    (type_qualifier)                             : (list $1)
    (type_qualifier_list type_qualifier)         : (append $1 (list $2))
    )

   (parameter_type_list
    (parameter_list)                             : (list 'parameter-type-list $1 #f)
    (parameter_list COMMA ELLIPSIS)              : (list 'parameter-type-list $1 #t)
    )

   (parameter_list
    (parameter_declaration)                      : (list $1)
    (parameter_list COMMA parameter_declaration) : (append $1 (list $3))
    )

   (parameter_declaration
    (declaration_specifiers declarator)          : (list $2 $1)
    (declaration_specifiers abstract_declarator) : (list $2 $1)
    (declaration_specifiers)                     : (list #f $1)
    )

   (IDENTIFIER_list
    (IDENTIFIER)                              : (list $1)
    (IDENTIFIER_list COMMA IDENTIFIER)        : (append $1 (list $3))
    )

   (type_name
    (specifier_qualifier_list)                      : (list 'type-name $1 #f)
    (specifier_qualifier_list abstract_declarator)  : (list 'type-name $1 $2)
    )

   (abstract_declarator
    (pointer)                                                : (list $1)
    (abstract_declarator2)                                   : (list $1)
    (pointer abstract_declarator2)                           : (list $1 $2)
    )

   (abstract_declarator2
    (LPAREN abstract_declarator RPAREN)                      : $2
    (LSBRA RSBRA)                                            : (list 'array  #f #f)
    (LSBRA expr RSBRA)                                       : (list 'array  #f $2)
    (abstract_declarator2 LSBRA RSBRA)                       : (list 'array  $1 #f)
    (abstract_declarator2 LSBRA expr RSBRA)                  : (list 'array  $1 $3)
    (LPAREN RPAREN)                                          : (list 'function #f #f)
    (LPAREN parameter_type_list RPAREN)                      : (list 'function #f $2)
    (abstract_declarator2 LPAREN RPAREN)                     : (list 'function $1 #f)
    (abstract_declarator2 LPAREN parameter_type_list RPAREN) : (list 'function $1 $3)
    )

   (initializer
    (assignment_expr)                     : $1
    (LCBRA RCBRA)                         : (list 'empty)
    (LCBRA initializer_list RCBRA)        : $2
    (LCBRA initializer_list COMMA RCBRA)  : $2
    )

   (initializer_list
    (initializer)                         : (list $1)
    (initializer_list COMMA initializer)  : (append $1 (list $3))
    (designation initializer)                        : (list $1 $2)               ; c99
    (initializer_list COMMA designation initializer) : (append $1 (list $3 $4))   ; c99
    )

   (designation
    (designator_list =)
    )

   (designator
    (LSBRA constant_expr RSBRA)
    (DOT IDENTIFIER)
    (DOT TYPE_NAME)
    )

   (designator_list
    (designator)                     : (list $1)
    (designator_list designator)     : (append $1 $2)
    )

   (statement
    (expression_statement)                : $1
    (labeled_statement)                   : $1
    (compound_statement)                  : $1
    (selection_statement)                 : $1
    (iteration_statement)                 : $1
    (jump_statement)                      : $1
    )

   (labeled_statement
    (IDENTIFIER COLON statement)          : (append (list 'SET-LABEL $1) $3)
    (CASE constant_expr COLON statement)  : (append (list 'CASE $2)      $4)
    (DEFAULT COLON statement)             : (append (list 'DEFAULT)      $3)
    )

   (compound_statement
    (LCBRA RCBRA)                                 : (list 'BLOCK #f)
    (LCBRA declaration_or_statement_list RCBRA)   : (list 'BLOCK $2)
    )

   (declaration_or_statement_list
    (declaration_or_statement)                                  : (list $1)
    (declaration_or_statement declaration_or_statement_list)    : (cons $1 $2)
    )

   (declaration_or_statement
    (declaration)                 : $1
    (type_definition)             : $1
    (statement)                   : $1
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
    )

   (selection_statement
    (IF LPAREN expr RPAREN statement)                  : (list 'IF $3 $5 #f)
    (IF LPAREN expr RPAREN statement ELSE statement)   : (list 'IF $3 $5 $7)
    (SWITCH LPAREN expr RPAREN statement)              : (list 'SWITCH $3 $5)
    )

   (iteration_statement
    (WHILE LPAREN expr RPAREN statement)                                         : (list 'WHILE $3 $5)
    (DO statement WHILE LPAREN expr RPAREN SEMICOLON)                            : (list 'DO $2 $5)
    (FOR LPAREN expression_statement expression_statement RPAREN statement)      : (list 'FOR $3 $4 #f $6)
    (FOR LPAREN expression_statement expression_statement expr RPAREN statement) : (list 'FOR $3 $4 $5 $7)
    )

   (jump_statement
    (GOTO IDENTIFIER SEMICOLON)  : (list 'GOTO $2)
    (CONTINUE SEMICOLON)         : (list 'CONTINUE)
    (BREAK SEMICOLON)            : (list 'BREAK)
    (RETURN SEMICOLON)           : (list 'RETURN #f)
    (RETURN expr SEMICOLON)      : (list 'RETURN $2)
    )

   (constant
    (INTEGER-CONSTANT)     : (list 'CONSTANT $1)
    (CHARACTER-CONSTANT)   : (list 'CONSTANT $1)
    (FLOAT-CONSTANT)       : (list 'CONSTANT $1)
    (DOUBLE-CONSTANT)      : (list 'CONSTANT $1)
    (LONG-DOUBLE-CONSTANT) : (list 'CONSTANT $1)
    )

   ))

;;;
;;;
;;;
(define (compile e) e)

(define type-table (make-hash-table 'eq?))
(define debug      (make-parameter #f))

(define (register-type id pointer declaration-specifiers)

  (define (check t x)
    ;;
    ;;  TODO: We likely need to resolve all the typef'ed
    ;;  types before comparing.  We just use equal? for now.
    ;;
    (if (debug)
      (if (equal? t x)
        (print "typedef: redefinition with the same definition: "  id)
        (print "typedef: redefinition with different definition: " id
               "\n  Previous: " t
               "\n This Time: "  x))))

  (define (register id t)
    (if (debug) (print "define-type: adding: " id " as: " t))
    (hash-table-put! type-table id t)
    (register-typedef-for-c89-scan id))

  (let ((t (cons pointer declaration-specifiers))
        (x (hash-table-get type-table id #f)))
    (if x (check t x))
    (register id t)))

(define (define-type typedef-declarator-list declaration-specifiers)
  (for-each (lambda (type-decl)
              (let ((name (car type-decl)))
                (register-type (string->symbol (token-string name))
                               (cdr type-decl)
                               declaration-specifiers)))
            typedef-declarator-list))

(provide "lang/c/c89-gram")
