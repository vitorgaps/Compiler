package domain.models;

public class Word extends Token{
    private String lexeme = "";
    
    public static final Word eq = new Word("=",Tag.EQ.getValor());
    public static final Word ne = new Word("<>",Tag.NE.getValor());
    public static final Word ls = new Word("<",Tag.LS.getValor());
    public static final Word le = new Word("<=",Tag.LE.getValor());
    public static final Word gr = new Word(">",Tag.GR.getValor());
    public static final Word ge = new Word(">=",Tag.GE.getValor());
    
    public Word (String s, int tag){
        super(tag);
        lexeme = s;
    }
    
    @Override
    public String toString(){
        return "" + lexeme;
    }
    
    public String getLexeme(){
        return "" + lexeme;
    }
    
    
}
