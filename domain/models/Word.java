package domain.models;

public class Word extends Token {
    private String lexeme = "";

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
