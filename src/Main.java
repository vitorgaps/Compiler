package src;

import src.domain.lexer.Lexer;
import src.domain.parser.Parser;
import src.domain.semantic.Semantic;

public class Main {

    public static void main(String[] args) throws Exception {

        if (args.length == 0) {
            System.out.println("É necessário passar o caminho do aquivo de teste. Exemplo: java main [source file]");
            return;
        }

        Lexer analisador = new Lexer(args[0]);
        Semantic semantic = new Semantic();

        Parser parser = new Parser(analisador,semantic);
        parser.program();

    }
}
