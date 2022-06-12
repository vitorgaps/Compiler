package src.domain.lexer.models;

public class Integer extends Token{
    public final int value;

    public Integer(int value){
        super(Tag.INTEGER.getValor());
        this.value = value;
    }
    
    @Override
    public String toString(){
        return "" + value;
    }    
}
