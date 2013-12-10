/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package br.scamtec.classes;

import java.awt.Component;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;

/**
 *
 * @author Paulo Henrique
 */
public class UtilData {

    private static Component rootPane;

    public static java.sql.Date stringToDate(String dataStr) {
        if (dataStr.equals("  /  /    ")) {
            return null;
        }



        try {
            SimpleDateFormat df = new SimpleDateFormat("dd/MM/yyyy");
            java.util.Date dataUtil = df.parse(dataStr);
            return new java.sql.Date(dataUtil.getTime());
        } catch (ParseException ex) {
            Logger.getLogger(UtilData.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static java.sql.Date stringToDateSemBarra(String dataStr) {
        if (dataStr.equals("  /  /    ")) {
            return null;
        }


        try {
            SimpleDateFormat df = new SimpleDateFormat("yyyyMMddhh");
            java.util.Date dataUtil = df.parse(dataStr);
            return new java.sql.Date(dataUtil.getTime());
        } catch (ParseException ex) {
            Logger.getLogger(UtilData.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public static String dateToString(java.util.Date dataUtil) {
        if (dataUtil == null) {
            return null;
        }
        try {
            SimpleDateFormat df = new SimpleDateFormat("dd/MM/yyyy");
            return df.format(dataUtil);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.print("ERRO");
        }
        return null;
    }

    public static String dateToStringSemBarra(java.util.Date dataUtil) {
        if (dataUtil == null) {
            return null;
        }
        try {
            SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");
            return df.format(dataUtil);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.print("ERRO");
        }
        return null;
    }

    public static String dateToStringComTraco(java.util.Date dataUtil) {
        if (dataUtil == null) {
            return null;
        }
        try {
            SimpleDateFormat df = new SimpleDateFormat("dd-MM-yyyy");
            return df.format(dataUtil);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.print("ERRO");
        }
        return null;
    }

    public static Date getDataCalendario(Date data) {
        DateFormat df = new SimpleDateFormat("yyyy/MM/dd/hh");
        java.sql.Date d = null;
        try {
            d = new java.sql.Date(df.parse(df.format(data)).getTime());

            return d;
        } catch (ParseException e) {
            JOptionPane.showMessageDialog(rootPane,
                    "Introduza a data correcta", "ERRO",
                    JOptionPane.ERROR_MESSAGE);
            return null;
        }
    }
}