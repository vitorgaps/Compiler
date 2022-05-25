import domain.exceptions.LexicError;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class MainTest {

    @Test
    void teste01() throws LexicError, IOException {
        System.out.printf("Teste 01");
        Main.main(new String[]{"testes/teste1.txt"});
    }

    @Test
    void teste02() throws LexicError, IOException {
        System.out.printf("Teste 02");
        Main.main(new String[]{"testes/teste2.txt"});
    }

    @Test
    void teste03() throws LexicError, IOException {
        System.out.printf("Teste 03");
        Main.main(new String[]{"testes/teste3.txt"});
    }

    @Test
    void teste04() throws LexicError, IOException {
        System.out.printf("Teste 04");
        Main.main(new String[]{"testes/teste4.txt"});
    }

    @Test
    void teste05() throws LexicError, IOException {
        System.out.printf("Teste 05");
        Main.main(new String[]{"testes/teste5.txt"});
    }

    @Test
    void teste06() throws LexicError, IOException {
        System.out.printf("Teste 06");
        Main.main(new String[]{"testes/teste6.txt"});
    }

    @Test
    void teste07() throws LexicError, IOException {
        System.out.printf("Teste 07");
        Main.main(new String[]{"testes/teste7.txt"});
    }

}