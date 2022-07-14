package src.domain.lexer.models;

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
    INTEGER(325),
    CARACTERE(326),


    // Arithmetic Operators
    ADD(265), // +
    SUB(266), // -
    MUL(267), // *
    DIV(268), // /

    //Symbols
    SEMICOLON(269), // ;
    COMMA(270), //  ,
    DOUBLE_QUOTES(271), // ""
    DOT(272), //  .
    LEFT_BRACKET(273), // (
    RIGHT_BRACKET(274), // )
    LEFT_BRACE(275), // {
    RIGHT_BRACE(276), // }

    //Operadores e pontuação
    OR(277),
    AND(278),
    EQ(279),
    GE(280),
    LE(281),
    NE(282),
    LS(283),
    GR(284),
    ASSIGN(285),

    UNDERLINE(286), // _
    COLON(287), // :
    ERROR(288);


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
