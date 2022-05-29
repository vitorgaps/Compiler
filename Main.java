import domain.exceptions.LexicError;
import domain.lexer.Lexer;
import domain.models.Tag;
import domain.models.Token;

import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException, LexicError {
        // TODO code application logic here
        Lexer analisador = new Lexer(args[0]);
        Token token = null;
        do {
                token = analisador.scan();
                if((char)token.tag!='\uffff') {
                    System.out.printf("Tag: %10s -> Token: %s\n", Tag.valueOf(token.tag), token.toString());
                }
        }while((char)token.tag!='\uffff');
        var table = analisador.getTable();
        System.out.println("Fim da execução");
        System.out.println("--------------------------------------------");
        System.out.println("Tabela Simbolos:");
        analisador.getTabelaSimbolos().forEach((i, v) -> System.out.print(v +", "));
    }
}
