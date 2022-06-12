package src;

import org.junit.jupiter.api.Test;

class MainTest {

    @Test
    void teste01() throws Exception {
        System.out.println("Teste 01");
        Main.main(new String[]{"src/testes/teste1.txt"});
    }

    @Test
    void teste02() throws Exception {
        System.out.println("Teste 02");
        Main.main(new String[]{"src/testes/teste2.txt"});
    }

    @Test
    void teste03() throws Exception {
        System.out.println("Teste 03");
        Main.main(new String[]{"src/testes/teste3.txt"});
    }

    @Test
    void teste04() throws Exception {
        System.out.println("Teste 04");
        Main.main(new String[]{"src/testes/teste4.txt"});
    }

    @Test
    void teste05() throws Exception {
        System.out.println("Teste 05");
        Main.main(new String[]{"src/testes/teste5.txt"});
    }

    @Test
    void teste06() throws Exception {
        System.out.println("Teste 06");
        Main.main(new String[]{"src/testes/teste6.txt"});
    }

    @Test
    void teste07() throws Exception {
        System.out.println("Teste 07");
        Main.main(new String[]{"src/testes/teste7.txt"});
    }

}