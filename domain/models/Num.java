package domain.models;

public class Num extends Token{
    public final int value;
    
    public Num(int value){
        super(Tag.NUM.getValor());
        this.value = value;
    }
    
    @Override
    public String toString(){
        return "" + value;
    }    
}
