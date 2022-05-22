import java.io.*;
import java.util.Hashtable;

/**
 *
 * @author vitor
 */
public class Lexer {
    public static int line = 1;
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
        reserve(new Word ("routine", Tag.ROUTINE));        
        reserve(new Word ("begin", Tag.BEG));
        reserve(new Word ("end", Tag.END));
        reserve(new Word ("declare", Tag.DECLARE));        
        reserve(new Word ("int", Tag.INT));
        reserve(new Word ("float", Tag.FLOAT));
        reserve(new Word ("char", Tag.CHAR));        
        reserve(new Word("if", Tag.IF));
        reserve(new Word("then", Tag.THEN));
        reserve(new Word("else", Tag.ELSE));
        reserve(new Word("repeat", Tag.REPEAT));
        reserve(new Word("until", Tag.UNTIL));
        reserve(new Word ("while", Tag.WHILE));
        reserve(new Word ("do", Tag.DO));
        reserve(new Word ("read", Tag.READ));
        reserve(new Word ("write", Tag.WRITE));
        reserve(new Word ("not", Tag.NOT));
        reserve(new Word ("or", Tag.OR));
        reserve(new Word ("and", Tag.AND)); 
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
    
    public Token scan() throws IOException{
        for(;; readch()){
            if(ch == ' ' || ch == '\t' || ch == '\r' || ch == '\b') continue;
            else if (ch == '\n') line++;
            else break;
        }
        
        switch(ch){
            //operators
            case '<':
                if (readch('=')) return new Word("<=", Tag.LE);
                if (ch == '>') return new Word ("<>", Tag.NE);
                else return new Token('<');            
            case '>':
                if (readch('=')) return new Word(">=", Tag.GE);
                else return new Token('>');                        
            case ':':
                if (readch('=')) return new Word(":=", Tag.EQ);
                else return new Token(':');
        }
        
        if (Character.isDigit(ch)){
            int value = 0;
            do{
                value = 10*value + Character.digit(ch,10);
                readch();
            } while(Character.isDigit(ch));
            return new Num(value);
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
            w = new Word (s, Tag.ID);
            words.put(s,w);
            return w;
        }
        
        Token t = new Token(ch);
        ch = ' ';
        return t;
    }
}
