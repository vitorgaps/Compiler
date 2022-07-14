package src.domain.semantic;

import src.domain.lexer.models.Identifier;
import src.domain.lexer.models.Tag;
import src.domain.lexer.models.Token;

import java.sql.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;

import static java.lang.System.in;

public class Semantic {
    public boolean verifyUnicy(Hashtable symbolsTable, String identifier) {
        return symbolsTable.containsKey(identifier);
    }

    public boolean compareTypes(Tag identifierType, Tag valueType) {
        if (identifierType.getValor() == Tag.NUM.getValor()) {
            return valueType.getValor() == Tag.NUM.getValor() || valueType.getValor() == Tag.INTEGER.getValor();
        } else {
            return identifierType == valueType;
        }
    }

    public Tag getResultingType(Tag firstType, Tag operandType, Tag secondType) {

        List<Tag> aritmeticOperands = new ArrayList<>(Arrays.asList(Tag.ADD, Tag.SUB, Tag.MUL, Tag.DIV));
        List<Tag> numericTypes = new ArrayList<>(Arrays.asList(Tag.INTEGER, Tag.NUM));
        List<Tag> relopOperands = new ArrayList<>(Arrays.asList(Tag.EQ, Tag.GR, Tag.GE, Tag.LS, Tag.LE, Tag.NE));

        if (relopOperands.contains(operandType)) {
            if (numericTypes.contains(firstType) && numericTypes.contains(secondType)) {
                return Tag.BOOL;
            } else if (firstType.equals(Tag.CARACTERE) && secondType.equals(Tag.CARACTERE)) {
                return operandType.equals(Tag.EQ) || operandType.equals(Tag.NE) ? Tag.BOOL : Tag.ERROR;
            } else {
                return Tag.ERROR;
            }
        }

        if (firstType.equals(Tag.ERROR) || operandType.equals(Tag.ERROR) || secondType.equals(Tag.ERROR)) {
            return Tag.ERROR;
        }

        if (operandType.equals(Tag.AND) || operandType.equals(Tag.OR)) {
            if (firstType != Tag.BOOL || secondType != Tag.BOOL) {
                return Tag.ERROR;
            } else {
                return Tag.BOOL;
            }
        }

        if (numericTypes.contains(firstType) && numericTypes.contains(secondType)) {
            if (firstType.equals(Tag.NUM) || secondType.equals(Tag.NUM)) {
                if (aritmeticOperands.contains(operandType)) {
                    return Tag.NUM;
                }
            }

            if (firstType.equals(Tag.INTEGER) && secondType.equals(Tag.INTEGER)) {
                if (operandType.equals(Tag.DIV)) {
                    return Tag.NUM;
                }
                if (aritmeticOperands.contains(operandType)) {
                    return Tag.INTEGER;
                } else {
                    return Tag.ERROR;
                }
            }
        }

        return firstType.equals(secondType) ? firstType : Tag.ERROR;
    }
}
