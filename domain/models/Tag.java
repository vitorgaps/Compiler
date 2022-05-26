package domain.models;

import java.util.HashMap;
import java.util.Map;

public enum Tag {

    //Palavras reservadas
    UNTIL(300),
    BEG(301),
    END(302),
    TYPE(303),
    INT(304),
    FLOAT(305),
    CHAR(306),
    BOOL(307),
    IF(308),
    ELSE(309),
    THEN(310),
    TRUE(311),
    FALSE(312),
    ROUTINE(313),
    READ(314),
    WRITE(315),
    FOR(316),
    WHILE(317),
    DECLARE(318),
    REPEAT(319),
    DO(320),
    NOT(321),
    LITERAL(322),

    //Outros tokens
    NUM(323),
    ID(324),

    //Operadores e pontuação
    OR(286),
    AND(287),
    EQ(288),
    GE(289),
    LE(290),
    NE(291),
    LS(292),
    GR(293),

    // Arithmetic Operators
    ADD(281), // +
    SUB(266), // -
    MUL(267), // *
    DIV(268), // /

    //Symbols

    SEMICOLON(278), // ;
    ASSIGN(279), //  =
    COMMA(280), //  ,
    DOUBLE_QUOTES(281), // ""
    DOT(282), //  .
    LEFT_BRACKET(283), // (
    RIGHT_BRACKET(284), // )
    LEFT_BRACE(284), // {
    RIGHT_BRACE(286), // }
    UNDERLINE(350);


    private int value;
    private static Map map = new HashMap<>();

    Tag(int value) {
        this.value = value;
    }

    static {
        for (Tag pageType : Tag.values()) {
            map.put(pageType.value, pageType);
        }
    }

    public static Tag valueOf(int pageType) {
        return (Tag) map.get(pageType);
    }

    public int getValor() {
        return value;
    }
}
