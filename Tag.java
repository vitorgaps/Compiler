package Compiler;

/**
 *
 * @author vitor
 */
public class Tag {
    public final static int
            
            //Palavras reservadas
            UNTIL = 256,
            BEG = 257,
            END = 258,
            TYPE = 259,
            INT = 260,
            FLOAT = 261,
            CHAR = 262,
            BOOL = 263,
            IF = 264,
            ELSE = 265,
            THEN = 266,
            TRUE = 267,
            FALSE = 268,
            ROUTINE = 269,
            READ = 270,
            WRITE = 271,
            FOR = 272,
            WHILE = 273,
            DECLARE = 274,
            REPEAT = 275,
            DO = 276,
            NOT = 277,
            
            //Operadores e pontuação
            OR = 286,
            AND = 287,
            EQ = 288,
            GE = 289,
            LE = 290,
            NE = 291,
            LS = 292,
            GR = 293,
            
            //Outros tokens
            NUM = 278,
            ID = 279;
}
