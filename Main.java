import domain.exceptions.LexicError;
import domain.lexer.Lexer;
import domain.models.Token;

import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException, LexicError {
        // TODO code application logic here
        Lexer analisador = new Lexer(args[0]);
        Token token = null;
        do {
                token = analisador.scan();
                System.out.printf("domain.models.Tag: %5d, domain.models.Token: %s\n", token.tag, token.toString());
        }while((char)token.tag!='\uffff');
        var table = analisador.getTable();
        System.out.println("Fim da execução");
    }
}
