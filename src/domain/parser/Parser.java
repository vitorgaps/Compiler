package src.domain.parser;

import src.domain.lexer.Lexer;
import src.domain.lexer.models.Tag;
import src.domain.lexer.models.Token;

public class Parser {
    private final Lexer lexer;  // lexical analyzer for this parser
    private Token look;

    public Parser(Lexer lexer) throws Exception {
        this.lexer = lexer;
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
        type();
        identList();
        match(Tag.SEMICOLON.getValor());
        if (look.tag == Tag.INT.getValor() || look.tag == Tag.CHAR.getValor() || look.tag == Tag.FLOAT.getValor()) {
            decl();
        }
    }

    private void type() throws Exception {
        if (look.tag == Tag.INT.getValor() || look.tag == Tag.CHAR.getValor() || look.tag == Tag.FLOAT.getValor()) {
            move();
        } else {
            error("Tipo não informado");
        }
    }

    private void identList() throws Exception {
        match(Tag.ID.getValor());
        if (look.tag == Tag.COMMA.getValor()) {
            move();
            identList();
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
            assingStmt();
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

    private void stmtSufix() throws Exception {
        match(Tag.UNTIL.getValor());
        expression();
    }

    private void repeatSmt() throws Exception {
        match(Tag.REPEAT.getValor());
        stmtList();
        stmtSufix();
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

    private void assingStmt() throws Exception {
        match(Tag.ID.getValor());
        match(Tag.ASSIGN.getValor());
        simpleExpr();
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

    private void expression() throws Exception {
        simpleExpr();
        if (isFirstRelop()) {
            relop();
            expression();
        }

    }

    private void simpleExpr() throws Exception {
        term();
        if (look.tag == Tag.ADD.getValor() || look.tag == Tag.SUB.getValor() || look.tag == Tag.OR.getValor()) {
            addOp();
            simpleExpr();
        }
    }

    private void term() throws Exception {
        factorA();
        if (look.tag == Tag.MUL.getValor() || look.tag == Tag.DIV.getValor() || look.tag == Tag.AND.getValor()) {
            mulOp();
            term();
        }
    }

    private void factorA() throws Exception {
        if (look.tag == Tag.NOT.getValor()) {
            move();
            factor();
        } else if (look.tag == Tag.SUB.getValor()) {
            move();
            factor();
        } else if (isFirstFactor()) {
            factor();
        } else {
            error("factorAExcption");
        }
    }

    private void factor() throws Exception {
        if (look.tag == Tag.ID.getValor()) {
            move();
        } else if (look.tag == Tag.LEFT_BRACKET.getValor()) {
            move();
            expression();
            match(Tag.RIGHT_BRACKET.getValor());
        } else if (isFirstConstant()) {
            constant();
        } else {
            error("factorException");
        }
    }

    private void relop() throws Exception {
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
    }

    private void mulOp() throws Exception {
        if (look.tag == Tag.MUL.getValor()) {
            move();
        } else if (look.tag == Tag.DIV.getValor()) {
            move();
        } else if (look.tag == Tag.AND.getValor()) {
            move();
        } else {
            error("mulopException");
        }
    }

    private void addOp() throws Exception {
        if (look.tag == Tag.ADD.getValor()) {
            move();
        } else if (look.tag == Tag.SUB.getValor()) {
            move();
        } else if (look.tag == Tag.OR.getValor()) {
            move();
        } else {
            error("addOpException");
        }
    }

    private void constant() throws Exception {
        if (look.tag == Tag.INTEGER.getValor()) {
            move();
        } else if (look.tag == Tag.NUM.getValor()) {
            move();
        } else if (look.tag == Tag.CARACTERE.getValor()) {
            move();
        } else {
            error("constantError");
        }

    }

    // GetFirt functions

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
