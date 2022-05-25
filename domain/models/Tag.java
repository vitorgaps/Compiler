package domain.models;

import java.util.HashMap;
import java.util.Map;

public enum Tag {

    //Palavras reservadas
    UNTIL(256),
    BEG(257),
    END(258),
    TYPE(259),
    INT(260),
    FLOAT(261),
    CHAR(262),
    BOOL(263),
    IF(264),
    ELSE(265),
    THEN(266),
    TRUE(267),
    FALSE(268),
    ROUTINE(269),
    READ(270),
    WRITE(271),
    FOR(272),
    WHILE(273),
    DECLARE(274),
    REPEAT(275),
    DO(276),
    NOT(277),
    LITERAL(278),

    //Operadores e pontuação
    OR(286),
    AND(287),
    EQ(288),
    GE(289),
    LE(290),
    NE(291),
    LS(292),
    GR(293),

    //Outros tokens
    NUM(279),
    ID(280);

//    // Arithmetic Operators
//    ADD(265), // +
//    SUB(266), // -
//    MUL(267), // *
//    DIV(268), // /
//
//    // Logical operators
//    EQUAL(269), // ==
//    NOT_EQUAL(270), // !=
//    LOWER(271), // <
//    LOWER_EQUAL(272), // <=
//    GREATER(273), // >
//    GREATER_EQUAL(274), // >=
//    AND(275), // &&
//    OR(276), // ||
//    NOT(277), // !
//
//    //Symbols
//
//    SEMICOLON(278), // ;
//    ASSIGN(279), //  =
//    COMMA(280), //  ,
//    DOUBLE_QUOTES(281), // ""
//    DOT(282), //  .
//    LEFT_BRACKET(283), // (
//    RIGHT_BRACKET(284), // )
//    LEFT_BRACE(284), // {
//    RIGHT_BRACE(286), // }


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
