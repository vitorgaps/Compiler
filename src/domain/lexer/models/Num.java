package src.domain.lexer.models;

public class Num extends Token{
    public final double value;
    
    public Num(double value){
        super(Tag.NUM.getValor());
        this.value = value;
    }
    
    @Override
    public String toString(){
        return "" + value;
    }    
}
