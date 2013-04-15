/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.classes;

/**
 *
 * @author paulo.henrique
 */
public class Teste {  
  
    /** 
     * @param args 
     */  
    public static void main(String[] args) {  
        try{       
            
 
            
               //Process p = Runtime.getRuntime().exec(new String[] { System.getProperty("/br/scamtec/classes") + "/"+ "scamtec.x"});  
               //Process p = Runtime.getRuntime().exec("/home2/paulo.henrique/NetBeansProjects/Scamtec/scamtec.x");  
               Runtime.getRuntime().exec("/home2/paulo.henrique/NetBeansProjects/Scamtec/scamtec.x");
               //System.out.println(p.exitValue());
              // if(p.exitValue()==0){       
                //  System.out.println("Programa terminou normalmente");       
               //}       
            }catch(Exception e){ 
                System.out.println("caiu no catch\n");
                e.printStackTrace();
                       
            }    
  
    }  
}  
