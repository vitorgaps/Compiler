package domain.lexer;

import domain.exceptions.LexicError;
import domain.models.*;
import domain.models.Integer;

import java.io.*;
import java.util.Hashtable;

public class Lexer {
    private int line = 1;
    private char ch = ' ';
    private FileReader file;    
    private Hashtable words = new Hashtable<>();
    
    private void reserve(Word w){
        words.put(w.getLexeme(), w);
    }
    
    public Lexer(String fileName) throws FileNotFoundException{
        try{
            file = new FileReader(fileName);
        }
        catch(FileNotFoundException e){
            System.out.println("Arquivo não encontrado");
            throw e;
        }
        
        //Inserção das palavras reservadas na hashTable
        reserve(new Word("routine", Tag.ROUTINE.getValor()));
        reserve(new Word("begin", Tag.BEG.getValor()));
        reserve(new Word("end", Tag.END.getValor()));
        reserve(new Word("declare", Tag.DECLARE.getValor()));
        reserve(new Word("int", Tag.INT.getValor()));
        reserve(new Word("float", Tag.FLOAT.getValor()));
        reserve(new Word("char", Tag.CHAR.getValor()));
        reserve(new Word("if", Tag.IF.getValor()));
        reserve(new Word("then", Tag.THEN.getValor()));
        reserve(new Word("else", Tag.ELSE.getValor()));
        reserve(new Word("repeat", Tag.REPEAT.getValor()));
        reserve(new Word("until", Tag.UNTIL.getValor()));
        reserve(new Word("while", Tag.WHILE.getValor()));
        reserve(new Word("do", Tag.DO.getValor()));
        reserve(new Word("read", Tag.READ.getValor()));
        reserve(new Word("write", Tag.WRITE.getValor()));
        reserve(new Word("not", Tag.NOT.getValor()));
        reserve(new Word("or", Tag.OR.getValor()));
        reserve(new Word("and", Tag.AND.getValor()));
    }
    
    private void readch() throws IOException{
        ch = (char) file.read();
    }
    
    private boolean readch(char c) throws IOException{
        readch();
        if (ch != c) return false;
        ch = ' ';
        return true;
    }
    
    public Hashtable getTable(){
        return words;
    }
    
    public Token scan() throws IOException, LexicError {

        for(;; readch()){
            if(ch == '%'){
                do{
                    if(readch('%')){
                        readch();
                        break;
                    }
                    if(ch=='\n'){
                        line++;
                    }

                    if(ch=='\uffff'){
                        throw new LexicError("Comentário não fechado", line);
                    }
                }while(ch!='%');
            }

            if(ch == ' ' || ch == '\t' || ch == '\r' || ch == '\b') continue;
            else if (ch == '\n') line++;
            else break;
        }

        switch(ch){
            //operators
            case '<':
                if (readch('=')) return new Word("<=", Tag.LE.getValor());
                if (ch == '>') return new Word("<>", Tag.NE.getValor());
                else return new Word("<", Tag.LS.getValor());
            case '>':
                if (readch('=')) return new Word(">=", Tag.GE.getValor());
                else return new Word(">", Tag.GR.getValor());
            case ':':
                if (readch('=')) return new Word(":=", Tag.EQ.getValor());
                else return new Word(":", Tag.COLON.getValor());
            case ';':
                readch();
                return new Word(";", Tag.SEMICOLON.getValor());
            case ',':
                readch();
                return new Word(",", Tag.COMMA.getValor());
            case '(':
                readch();
                return new Word("(", Tag.LEFT_BRACKET.getValor());
            case ')':
                readch();
                return new Word(")", Tag.RIGHT_BRACKET.getValor());
            case '+':
                readch();
                return new Word("+", Tag.ADD.getValor());
            case '-':
                readch();
                return new Word("-", Tag.SUB.getValor());
            case '*':
                readch();
                return new Word("*", Tag.MUL.getValor());
            case '/':
                readch();
                return new Word("/", Tag.DIV.getValor());
            case '{':
                readch();
                return new Word("{", Tag.LEFT_BRACE.getValor());
            case '}':
                readch();
                return new Word("}", Tag.RIGHT_BRACE.getValor());
        }
        
        if(ch =='"'){
            StringBuffer sb = new StringBuffer();
            do{
                sb.append(ch);
                readch();                
                //Tratar EOF
                if(ch=='\uffff'){
                    throw new LexicError("String não fechada", line);
                }
            }while(ch!='"');  
            sb.append(ch);
            readch();
            String s = sb.toString();
            Word w = new Word(s, Tag.LITERAL.getValor());
            return w;            
        }       
        
        if (Character.isDigit(ch)){
            double value = 0;
            do{
                value = 10 * value + Character.digit(ch,10);
                readch();
                if(ch=='.') {
                    StringBuffer sb = new StringBuffer();
                    readch();

                    if(Character.isDigit(ch))
                        sb.append(".");
                    else
                        throw new LexicError("Float mal formatado",line);

                    do{
                        sb.append(ch);
                        readch();
                    }while(Character.isDigit(ch));
                    double decimal = Double.parseDouble(sb.toString());
                    value = value + decimal;
                    return new Num(value);
                }
            } while(Character.isDigit(ch));
            return new Integer((int)value);
        }
        
        if (Character.isLetter(ch)){
            StringBuffer sb = new StringBuffer();
            do{
                sb.append(ch);
                readch();                
            }while(Character.isLetterOrDigit(ch));
            
            String s = sb.toString();
            Word w = (Word)words.get(s);
            if (w!= null) return w;
            w = new Word(s, Tag.ID.getValor());
            words.put(s,w);
            return w;
        }
        
        Token t = new Token(ch);
        ch = ' ';
        return t;
    }
}
