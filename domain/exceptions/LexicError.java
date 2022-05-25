package domain.exceptions;

public class LexicError extends Exception {
            
    public LexicError(String message, int line){
        super(String.format("%s, linha %s", message, line));        
    }
    
}
