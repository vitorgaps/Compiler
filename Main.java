package Compiler;
import java.io.IOException;

/**
 *
 * @author vitor
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws IOException{
        // TODO code application logic here
        System.out.println("File");
        Lexer analisador = new Lexer("teste.txt");
        analisador.scan();
    }
    
}
