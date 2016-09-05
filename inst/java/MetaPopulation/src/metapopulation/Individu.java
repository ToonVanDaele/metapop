/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metapopulation;

/**
 *
 * @author Aranka
 */
public class Individu implements Comparable<Individu> {
    private int age;
    private String name;
    private boolean sex; //True=Male False= Female
    
    /**
     * Constructor for an instance of Individu.
     * @param age the starting age of the individual
     * @param name a name for the individual
     * @param sex the sex of the individual: True = Male False = Female
     */
    
    public Individu(int age, String name, boolean sex){
        this.age = age;
        this.name = name;
        this.sex = sex;
    }
    
    /**
     * 
     * @return the age of the individual
     */
    public int getAge(){
        return age;
    }
    /**
     * 
     * @return the name of the individual
     */
    public String getName(){
        return name;
    }
    /**
     * 
     * @return the sex of the individual
     */
    public boolean getSex(){
        return sex;
    }
    /**
     * Let the individual age by one year (or other unit of age)
     */
    public void ageing(){
        age +=1;
    }
    
    /**
     * Set the age of the individual
     * @param age the wanted age
     */
    public void setAge(int age){
        this.age = age;
    }
    /**
     * Set the name of the individual
     * @param name the wanted name
     */
    public void setName(String name){
        this.name = name;
    }
    /**
     * Set the sex of the individual
     * @param sex the wanted sex: True = Male False = Female
     */
    public void setSex(boolean sex){
        this.sex = sex;
    }
    /**
     * Change the comparator of the class Individu.
     * @param o the individual to compare to
     * @return -1 smaller, 0 equal, 1 bigger than o.
     */
    @Override
    public int compareTo(Individu o) {
        int sexCompare = Boolean.valueOf(getSex()).compareTo(o.getSex());
        if(sexCompare != 0){
            return sexCompare;
        }else{
            int ageCompare = Integer.valueOf(getAge()).compareTo(o.getAge());
            if(ageCompare !=0){
                return ageCompare;
            }else{
                return getName().compareTo(o.getName()); 
            }
        }   
    }
    
   
    
}
