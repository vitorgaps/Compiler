package src.domain.parser;

import src.domain.lexer.Lexer;
import src.domain.lexer.models.Identifier;
import src.domain.lexer.models.Tag;
import src.domain.lexer.models.Token;
import src.domain.semantic.Semantic;

public class Parser {
    private final Lexer lexer;  // lexical analyzer for this parser
    private final Semantic semantic;
    private Token look;

    public Parser(Lexer lexer, Semantic semantic) throws Exception {
        this.lexer = lexer;
        this.semantic = semantic;
        move();
    }

    void move() throws Exception {
        look = lexer.scan();
    }

    void error(String s) {
        throw new Error(String.format("Erro próximo a linha %s. Erro: %s", lexer.line, s));
    }

    void match(int t) throws Exception {
        if (look.tag == t) {
            move();
        } else {
            error(String.format("Syntax error. Expect Tag : %s. Read Tag: %s", Tag.valueOf(t), Tag.valueOf(look.tag)));
        }
    }

    public void program() throws Exception {
        match(Tag.ROUTINE.getValor());
        body();
    }

    private void body() throws Exception {
        if (look.tag == Tag.DECLARE.getValor()) {
            declList();
        }

        if (look.tag == Tag.BEG.getValor()) {
            match(Tag.BEG.getValor());
            stmtList();
            match(Tag.END.getValor());
            if(((char) look.tag !='\uffff')) {
                error("Esperado fim de arquivo.");
            }
        } else {
            error("Token esperado: begin ou declare");
        }
    }

    private void declList() throws Exception {
        match(Tag.DECLARE.getValor());
        decl();
    }

    private void decl() throws Exception {
        Tag type = type();
        identList(type);
        match(Tag.SEMICOLON.getValor());
        if (look.tag == Tag.INT.getValor() || look.tag == Tag.CHAR.getValor() || look.tag == Tag.FLOAT.getValor()) {
            decl();
        }
    }

    private Tag type() throws Exception {
        if (look.tag == Tag.INT.getValor() || look.tag == Tag.CHAR.getValor() || look.tag == Tag.FLOAT.getValor()) {
            Tag type = Tag.valueOf(look.tag);
            move();
            return type;
        } else {
            error("Incorrect type");
        }
        return Tag.TYPE;
    }

    private void identList(Tag type) throws Exception {
        if(look.tag == Tag.ID.getValor()){
            if(semantic.verifyUnicy(lexer.getTabelaSimbolos(),((Identifier)look).getLexeme())){
                if(((Identifier)look).getType()!=Tag.TYPE){
                    error("Identifier has already been declared");
                }
                else{
                    ((Identifier)look).assignType(type);
                }
            }
        }
        match(Tag.ID.getValor());

        if (look.tag == Tag.COMMA.getValor()) {
            move();
            identList(type);
        }
    }

    private void stmtList() throws Exception {
        stmt();
        if (look.tag == Tag.SEMICOLON.getValor()) {
            match(Tag.SEMICOLON.getValor());
            stmtList();
        }
    }

    private void stmt() throws Exception {
        if (look.tag == Tag.ID.getValor()) {
            assignStmt(((Identifier)look).getType());
        } else if (look.tag == Tag.IF.getValor()) {
            ifStmt();
        } else if (look.tag == Tag.WHILE.getValor()) {
            whileStmt();
        } else if (look.tag == Tag.REPEAT.getValor()) {
            repeatSmt();
        } else if (look.tag == Tag.READ.getValor()) {
            readStmt();
        } else if (look.tag == Tag.WRITE.getValor()) {
            writeStmt();
        }
    }

    private void stmtSuffix() throws Exception {
        match(Tag.UNTIL.getValor());
        expression();
    }

    private void repeatSmt() throws Exception {
        match(Tag.REPEAT.getValor());
        stmtList();
        stmtSuffix();
    }

    private void ifStmt() throws Exception {
        match(Tag.IF.getValor());
        expression();
        match(Tag.THEN.getValor());
        stmtList();
        if (look.tag == Tag.END.getValor()) {
            match(Tag.END.getValor());
        } else if (look.tag == Tag.ELSE.getValor()) {
            match(Tag.ELSE.getValor());
            stmtList();
            match(Tag.END.getValor());
        } else {
            error("ifStmtException");
        }
    }

    private void assignStmt(Tag type) throws Exception {
        if(lexer.getTabelaSimbolos().containsKey(((Identifier)look).getLexeme())){
            if(((Identifier)look).getType().equals(Tag.TYPE))
                error("Identifier does not exists");
        }
        match(Tag.ID.getValor());
        match(Tag.ASSIGN.getValor());
        Tag expressionType = simpleExpr();
        if(!semantic.compareTypes(type, expressionType)){
            error("Incompatible types assign");
        }
    }

    private void whileStmt() throws Exception {
        stmtPrefix();
        stmtList();
        match(Tag.END.getValor());
    }

    private void stmtPrefix() throws Exception {
        match(Tag.WHILE.getValor());
        expression();
        match(Tag.DO.getValor());
    }

    private void readStmt() throws Exception {
        match(Tag.READ.getValor());
        match(Tag.LEFT_BRACKET.getValor());
        if(lexer.getTabelaSimbolos().containsKey(((Identifier)look).getLexeme())){
            Identifier symbol = (Identifier) lexer.getTabelaSimbolos().get(((Identifier) look).getLexeme());
            if(symbol.getType().equals(Tag.TYPE)){
                error("Identifier does not exists");
            }
        }
        match(Tag.ID.getValor());
        match(Tag.RIGHT_BRACKET.getValor());
    }

    private void writeStmt() throws Exception {
        match(Tag.WRITE.getValor());
        match(Tag.LEFT_BRACKET.getValor());
        writable();
        match(Tag.RIGHT_BRACKET.getValor());
    }

    private void writable() throws Exception {
        if (look.tag == Tag.LITERAL.getValor()) {
            move();
        } else {
            simpleExpr();
        }
    }

    private Tag expression() throws Exception {
        Tag firstExpressionType = simpleExpr();
        Tag returnType = firstExpressionType;

        if (isFirstRelop()) {
            Tag relopType = relop();
            Tag secondExpressionType = expression();
            Tag resultingType = semantic.getResultingType(firstExpressionType,relopType,secondExpressionType);
            returnType = resultingType;
            if(!resultingType.equals(Tag.BOOL)){
                error("Relop operations can't be performed between " + firstExpressionType + " and " + secondExpressionType +" types");
            }
        }

        return returnType;
    }

    private Tag simpleExpr() throws Exception {
        Tag termType = term();
        Tag returnType = termType;

        if (look.tag == Tag.ADD.getValor() || look.tag == Tag.SUB.getValor() || look.tag == Tag.OR.getValor()) {
           Tag addOpType = addOp();

            Tag expressionType = simpleExpr();

           returnType = semantic.getResultingType(termType,addOpType,expressionType);
           if(addOpType==Tag.OR){
               if(returnType.equals(Tag.ERROR)){
                   error("Cannot do an or between an " + termType + " and a " + expressionType);
               }
           } else{
               if(returnType.equals(Tag.ERROR)){
                   error("Semantic error");
               }
           }
        }

        return returnType;
    }

    private Tag term() throws Exception {
        Tag factorAType = factorA();
        Tag returnType = factorAType;

        if (look.tag == Tag.MUL.getValor() || look.tag == Tag.DIV.getValor() || look.tag == Tag.AND.getValor()) {
            Tag mulOpType = mulOp();
            Tag termType = term();
            returnType = semantic.getResultingType(factorAType,mulOpType,termType);
            if(mulOpType==Tag.AND){
                if(returnType.equals(Tag.ERROR)){
                    error("Cannot do an and between an " + termType + " and a " + factorAType);
                }
            } else{
                if(returnType.equals(Tag.ERROR)){
                    error("Semantic error");
                }
            }
        }
        return returnType;
    }

    private Tag factorA() throws Exception {
        Tag returnType = Tag.ERROR;

        if (look.tag == Tag.NOT.getValor()) {
            move();
            Tag factorType = factor();
            returnType = factorType;
            if(!factorType.equals(Tag.BOOL)){
                error("Not cannot be performed with a " + factorType +" type");
            }
        } else if (look.tag == Tag.SUB.getValor()) {
            move();
            Tag factorType = factor();
            returnType = factorType;
            if(!factorType.equals(Tag.INTEGER) && !factorType.equals(Tag.NUM)){
                error("- cannot be applied to " +factorType+ "type");
            }
        } else if (isFirstFactor()) {
            returnType = factor();
        } else {
            error("factorAException");
        }

        return returnType;
    }

    private Tag factor() throws Exception {
        Tag returnType = Tag.ERROR;
        if (look.tag == Tag.ID.getValor()) {
            if(lexer.getTabelaSimbolos().containsKey(((Identifier)look).getLexeme())){
                Identifier symbol = (Identifier) lexer.getTabelaSimbolos().get(((Identifier) look).getLexeme());
                if(symbol.getType().equals(Tag.TYPE)){
                    error("Identifier does not exists");
                }
            }

            returnType = ((Identifier)look).getType();
            move();
        } else if (look.tag == Tag.LEFT_BRACKET.getValor()) {
            move();
            returnType = expression();
            match(Tag.RIGHT_BRACKET.getValor());
        } else if (isFirstConstant()) {
            returnType = constant();
        } else {
            error("factorException");
        }

        return returnType;
    }

    private Tag relop() throws Exception {
        Tag returnType = Tag.valueOf(look.tag);
        if (look.tag == Tag.EQ.getValor()) {
            move();
        } else if (look.tag == Tag.GR.getValor()) {
            move();
        } else if (look.tag == Tag.GE.getValor()) {
            move();
        } else if (look.tag == Tag.LS.getValor()) {
            move();
        } else if (look.tag == Tag.LE.getValor()) {
            move();
        } else if (look.tag == Tag.NE.getValor()) {
            move();
        } else {
            error("relopException");
        }
        return returnType;
    }

    private Tag mulOp() throws Exception {
        Tag returnType = Tag.valueOf(look.tag);
        if (look.tag == Tag.MUL.getValor()) {
            move();
        } else if (look.tag == Tag.DIV.getValor()) {
            move();
        } else if (look.tag == Tag.AND.getValor()) {
            move();
        } else {
            error("mulopException");
        }
        return returnType;
    }

    private Tag addOp() throws Exception {
        Tag returnTag = Tag.valueOf(look.tag);
        if (look.tag == Tag.ADD.getValor()) {
            move();
        } else if (look.tag == Tag.SUB.getValor()) {
            move();
        } else if (look.tag == Tag.OR.getValor()) {
            move();
        } else {
            error("addOpException");
        }
        return returnTag;
    }

    private Tag constant() throws Exception {
        Tag returnType = Tag.valueOf(look.tag);
        if (look.tag == Tag.INTEGER.getValor()) {
            move();
        } else if (look.tag == Tag.NUM.getValor()) {
            move();
        } else if (look.tag == Tag.CARACTERE.getValor()) {
            move();
        } else {
            error("constantError");
        }

        return  returnType;
    }

    private boolean isFirstConstant() {
        return look.tag == Tag.INTEGER.getValor()
                || look.tag == Tag.CARACTERE.getValor()
                || look.tag == Tag.NUM.getValor();
    }

    private boolean isFirstFactor() {
        return isFirstConstant() || look.tag == Tag.ID.getValor() || look.tag == Tag.LEFT_BRACKET.getValor();
    }

    private boolean isFirstFactorA() {
        return isFirstFactor() || look.tag == Tag.NOT.getValor() || look.tag == Tag.SUB.getValor();
    }

    private boolean isFirstRelop() {
        return (look.tag == Tag.EQ.getValor())
                || (look.tag == Tag.GR.getValor())
                || (look.tag == Tag.GE.getValor())
                || (look.tag == Tag.LS.getValor())
                || (look.tag == Tag.LE.getValor())
                || (look.tag == Tag.NE.getValor());
    }
}
