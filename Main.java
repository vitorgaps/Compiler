import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException, LexicError{
        // TODO code application logic here
        Lexer analisador = new Lexer("teste.txt");
        Token token = null;
        do {
                token = analisador.scan();
                System.out.printf("Tag: %5d, Token: %s\n", token.tag, token.toString());           
        }while((char)token.tag!='\uffff');
        var table = analisador.getTable();
        System.out.println("Fim da execução");
    }
}
