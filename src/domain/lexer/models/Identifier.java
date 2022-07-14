package src.domain.lexer.models;

public class Identifier extends Token{
    private String lexeme = "";
    private Tag type;
    private Object value;

    public Identifier(String s) {
        super(Tag.ID.getValor());
        this.type = Tag.TYPE;
        lexeme = s;
    }

    public Tag getType(){
        return this.type;
    }

    public Object getValue(){
        return this.value;
    }

    public String getLexeme() {
        return lexeme;
    }

    public void assignType(Tag type){
        if(type == Tag.INT)
            this.type = Tag.INTEGER;
        if(type == Tag.FLOAT)
            this.type = Tag.NUM;
        if(type == Tag.CHAR)
            this.type = Tag.CARACTERE;
    }

    public void assignValue(Integer value){
        this.value = value;
    }

    public void assignValue(Float value){
        this.value = value;
    }

    public void assignValue(Character value){
        this.value = value;
    }

    @Override
    public String toString() {return "" + value;}
}
