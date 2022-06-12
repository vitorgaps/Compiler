package src;

import src.domain.lexer.Lexer;
import src.domain.parser.Parser;

public class Main {

    public static void main(String[] args) throws Exception {

        if (args.length == 0) {
            System.out.println("É necessário passar o caminho do aquivo de teste. Exemplo: java main [source file]");
            return;
        }

        Lexer analisador = new Lexer(args[0]);

        Parser parser = new Parser(analisador);
        parser.program();

    }
}
