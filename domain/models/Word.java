package domain.models;

public class Word extends Token {
    private String lexeme = "";

    public static final Word eq = new Word("=", Tag.EQ.getValor());
    public static final Word ne = new Word("<>", Tag.NE.getValor());
    public static final Word ls = new Word("<", Tag.LS.getValor());
    public static final Word le = new Word("<=", Tag.LE.getValor());
    public static final Word gr = new Word(">", Tag.GR.getValor());
    public static final Word ge = new Word(">=", Tag.GE.getValor());
    public static final Word sum = new Word("+", Tag.ADD.getValor());
    public static final Word min = new Word("-", Tag.SUB.getValor());
    public static final Word mult = new Word("*", Tag.MUL.getValor());
    public static final Word div = new Word("/", Tag.DIV.getValor());
    public static final Word and = new Word("&&", Tag.AND.getValor());
    public static final Word or = new Word("||", Tag.OR.getValor());
    public static final Word not = new Word("!", Tag.NOT.getValor());
    public static final Word semicolon = new Word(";", Tag.SEMICOLON.getValor());
    public static final Word assign = new Word("=", Tag.ASSIGN.getValor());
    public static final Word comma = new Word(",", Tag.COMMA.getValor());
    public static final Word quotes = new Word("\"\"", Tag.DOUBLE_QUOTES.getValor());
    public static final Word dot = new Word(".", Tag.DOT.getValor());
    public static final Word lbraket = new Word("(", Tag.LEFT_BRACKET.getValor());
    public static final Word rbraket = new Word(")", Tag.RIGHT_BRACKET.getValor());
    public static final Word lbrace = new Word("{", Tag.LEFT_BRACE.getValor());
    public static final Word rbrace = new Word("}", Tag.RIGHT_BRACE.getValor());
    public static final Word underline = new Word("_", Tag.UNDERLINE.getValor());

    public Word(String s, int tag) {
        super(tag);
        lexeme = s;
    }

    @Override
    public String toString() {
        return "" + lexeme;
    }

    public String getLexeme() {
        return "" + lexeme;
    }


}
