
import java.io.IOException;

/**
 *
 * @author vitor
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws IOException{
        // TODO code application logic here
        Lexer analisador = new Lexer("teste.txt");
        Token token;
        do {
           token = analisador.scan();
           System.out.println("Tokens = " + token.toString());
        }while(token.tag!=65535);
        var table = analisador.getTable();
        System.out.println("Fim da execução");
    }
    
}
