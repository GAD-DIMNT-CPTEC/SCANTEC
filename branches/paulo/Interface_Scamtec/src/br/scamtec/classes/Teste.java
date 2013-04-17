/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.classes;

import java.io.BufferedReader;
import java.io.FileReader;
import javax.swing.JOptionPane;

/**
 *
 * @author paulo.henrique
 */
public class Teste {  
  
    
    public boolean procura() {  
    boolean achou = false;  
    String NomeArq="scamtec.conf";  
    String linha="";
    String [] vetor=null;
    String pal = JOptionPane.showInputDialog("Digite o nome da palavra a procurar:");       
    try {  
        BufferedReader in = new BufferedReader(new FileReader("scamtec.conf"));  
        while ((linha = in.readLine()) != null) {  
            if (linha.contains(pal)) {  
                vetor = linha.split(" ");
                System.out.println(linha);  
                achou = true;  
            }  
        }       
        
    } catch (Exception e) {  
        System.err.println("Erro na abertura do arquivo " + NomeArq);  
        return achou;  
    }  
    return achou;  
}   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    /** 
     * @param args 
     */  
    public static void main(String[] args) {  
        new Teste().procura();
        
  
    }  
}  
