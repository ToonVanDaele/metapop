/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metapopulation;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import jdistlib.Binomial;
import jdistlib.Normal;
import jdistlib.Poisson;


/**
 *
 * @author Aranka
 */
public class Populatie {
    private int maxAge;
    private int patchNr;
    private double[] overlevingsKans;
    //private double[][] stochOverlevingsKans;
    private double ha;
    private int k;
    private int ccInfluenceAge;
    //private double kSD;
    private SortedSet<Individu> inwoners; // of Set?
    private int kinderen;
    //private double[] stochKinderen;
    private double voortplanting;
    //private double[] stochVoortplanting;
    private double sexRatio;
    private int reproductiveAge;
    private Random rg;
    private int idCounter;
    
    /**
     * Constructor for an instance of the class Populatie.
     * This constructor creates an instance in which non of the variables are specified yet.
     */
    public Populatie(){
        this.inwoners = new TreeSet<>();
        this.rg = new Random();
        this.idCounter = 1;
    }
    
    /**
     * Constructor for an instance of the class Populatie.
     * This constructor creates an instance with all necessary variables specified
     * @param maxAge the maximal age individuals in this population can reach before they die
     * @param patchNr the ID of the patch this population represents
     * @param repAge the reproductive age
     * @param survChance an array with survival chances. Position i corresponds to age i
     * @param ha the surface area of the patch
     * @param ccAge the minimal age that is influenced by density dependence
     * @param kinderen the mean number of offspring.
     * @param voortplanting the chance of mating in in one time-step
     * @param sexRatio the proportion males in the population
     */
    public Populatie(int maxAge, int patchNr, int repAge, double[] survChance, double ha, int ccAge, int kinderen, double voortplanting, double sexRatio){
        this.maxAge = maxAge;
        this.patchNr = patchNr;
        this.reproductiveAge = repAge;
        this.overlevingsKans = survChance;
        //this.stochOverlevingsKans = new double[repAge][2];
        this.inwoners = new TreeSet<>();
        this.ha = ha;
        this.k = (int) Math.round(ha*5);
        this.ccInfluenceAge = ccAge;
        //this.kSD = kSD;
        this.kinderen = kinderen;
        //this.stochKinderen = kinderenSD;
        this.voortplanting = voortplanting;
        //this.stochVoortplanting = voortplantingSD;
        this.sexRatio = sexRatio;
        this.rg = new Random(420*patchNr);
        this.idCounter = 1;
    }
    /**
    * Creates a random initial population with males and females divided according to the sexRatio, 
    * random names and the pre-specified age distribution
    * @param populatiegrootte an array with in position i the number of individuals of age i
    */
    public void initialPopulation(int[] populatiegrootte){
        if(populatiegrootte.length >maxAge){
            throw new MaxAgeException("lenght of array can't be bigger than" + maxAge);
        }
        if(populatiegrootte.length > 0){
            for(int i=0;i<populatiegrootte.length;i++){
                for(int j=0;j<populatiegrootte[i]; j++){
                    boolean sex = randomDraw(sexRatio);  
                    String naam;
                    naam = patchNr + ":" + idCounter;
                    idCounter +=1;
                    inwoners.add(new Individu(i,naam,sex));
                }
            
            }
        }
    }
    
    /**
     * Creates an initial population with pre-specified ages and sexes, but random names.
     * Both arrays should be of the same size, the first individual will get the first age and first sex etc.
     * @param ages an array containing the ages of the individuals
     * @param sexes an array containing the sexes of the individuals True = Male False = Female.
     */
    public void initialPopulation(int[] ages, boolean[] sexes){
        for(int i=1;i<ages.length;i++){
            String naam;
            naam = patchNr + ":" + idCounter;
            idCounter +=1;
            inwoners.add(new Individu(ages[i], naam, sexes[i]));
        }
    }
    
    public void setSurvival(double[] newSurvival){
        this.overlevingsKans = newSurvival;
    }
    
    public void setReproduction(double newRep){
        this.voortplanting = newRep;
    }
    
    /**
     * Add a set of individuals to the population.
     * If an individual of the set you want to add is already in the population this individual will not be added.
     * @param individuals a Set of individuals (class Individu) to add to the population
     */
    public void addIndividuals(Set<Individu> individuals){
        inwoners.addAll(individuals);
    }
    
    /**
     * Replicates the process of reproduction in the population.
     * As soon as there is one male individual present in the population all females can reproduce.
     * The proportion that will successfully reproduce is given by the parameter voortplanting.
     */
    public void reproduction(){
        //double reprProportion = demographicStoch(voortplanting, stochVoortplanting[1]);
        // Get number reproducting individuals
        //int nrReproduce = (int) Math.round(voortplanting*Math.min(reproductionCount()[0], reproductionCount()[1]));
        int nrReproduce = 0;
        //if(reproductionCount()[1]>0){
            nrReproduce = (int) Math.round(voortplanting*reproductionCount()[0]);
        //}
        Poisson pois = new Poisson(kinderen); // initialise a poisson distribution simulator
        for(int i = 0; i<nrReproduce;i++){
            int aantalJongen = (int) pois.random(); // get number of offspring for the individual
            for(int j=0;j<aantalJongen;j++){ 
                // create new offspring
                String naam;
                naam = patchNr + ":" + idCounter;
                idCounter +=1;
                inwoners.add(new Individu(0,naam,randomDraw(sexRatio)));
            }
        }
        
        
    }
    
    /**
     * Checks which individuals will die in a time-step.
     * the array overlevingsKans contains probabilities up until the reproductive age. 
     * From then on an individual always has the same survival chances.
     * Individuals who have reached the maximum age die. 
     */
    public void mortality(){
        Iterator<Individu> it = inwoners.iterator();
        while(it.hasNext()){
            Individu inwoner = it.next();
            if(inwoner.getAge() <= reproductiveAge){
                double faith = rg.nextDouble();
                if(overlevingsKans[inwoner.getAge()]<faith){
                    //System.out.println(inwoner.getName()+ ": Natuurlijke dood");
                    it.remove();
                }
            }else{
                if(inwoner.getAge()>=maxAge){
                    //System.out.println(inwoner.getName()+ ": Maximum leeftijd");
                    it.remove();
                }else{
                    double faith = rg.nextDouble();
                    if(overlevingsKans[reproductiveAge]<faith){
                        //System.out.println(inwoner.getName()+ ": Natuurlijke dood");
                        it.remove();
                    }
                }
            }
        }
    }
    /**
     * Simulate dispersal.
     * This method decides for each individual, whether it will stay put or migrate to another patch and if so, to which patch
     * @param migration an array with migration probabilities. element i is the probability of migration to patch i
     * @return an ArrayList with on position i a Set of individuals that will migrate to patch i
     */
    public ArrayList<HashSet<Individu>> dispersal(Double[] migration){
        // make a double-array in stead of a Double-array.
        double[] dmigration = new double[migration.length];
        for(int i=0;i<migration.length;i++){
            dmigration[i]=migration[i];
        }
        // Initiate output
        ArrayList<HashSet<Individu>> migratieLijst = new ArrayList<>(dmigration.length);
        for(int i=0;i<dmigration.length;i++){
            migratieLijst.add(new HashSet<>());
        }
        // determine migration
        Iterator<Individu> it = inwoners.iterator();
        while(it.hasNext()){
            Individu inwoner = it.next();
            if(inwoner.getAge()>= reproductiveAge){
                int faith = multiDraw(dmigration);
                if(faith != patchNr){
                   migratieLijst.get(faith).add(inwoner);
                   it.remove();
                }
            }
        }
        return migratieLijst;
    }
    
    /**
     * Check whether the carrying capacity is reached and let individuals die if this is the case.
     */
    public void carryingCapacity(){
        // count how many individuals there are on which carrying capacity is of influence
        int influence = 0;
        for(Individu inwoner:inwoners){
            if(inwoner.getAge()>=ccInfluenceAge ){
                influence +=1;
            }
        }
        // if carrying capacity is reached, choose random individuals who will die untill balance is restored
        if(influence>k){
            double kd = (double) k;
            double surv = kd/influence;
            //System.out.println("cc mortality = " + surv);
            Iterator<Individu> it = inwoners.iterator();
            while(it.hasNext()){
                Individu inwoner = it.next();
                if(inwoner.getAge()>= ccInfluenceAge  &&!randomDraw(surv)){
                    //System.out.println(inwoner.getName()+ ": No territory" + inwoner.getSex());
                    it.remove();
                }
            }
        }
    }
    
    /**
     * Increases each individual's age.
     */
    public void ageing(){
        Iterator<Individu> it = inwoners.iterator();
        while(it.hasNext()){ 
            it.next().ageing();
        }
    }
    
    /**
     * Changes the survival and reproduction chances to simulate a catastrophe
     * @param severity parameter smaller than 1 by which the survival and reproductive rates are multiplied
     */
    public void catastrophe(double severity){
        this.voortplanting *= severity;
        for(double d:this.overlevingsKans){
            d *= severity;
        }
    }
    /**
     * Changes the survival and reproduction chances to simulate a catastrophe
     * @param severity an array that contains in the first position the change in reproductive rate and in the second position the change in survival rate.
     */
    public void catastrophe(double[] severity){
        this.voortplanting *= severity[0];
        for(double d:this.overlevingsKans){
            d *= severity[1];
        }
    }
    
    /**
     * Method to restore survival and reproductive chances to their original values
     * @param voortplanting original reproductive rate
     * @param overleving original survival rate
     */
    public void restore(double voortplanting, double[] overleving){
        this.voortplanting=voortplanting;
        this.overlevingsKans = overleving;
    }
    
    /**
     * Print information about the population on the console.
     */
    public void printPopulation(){
        for(Individu inwoner:inwoners){
            String geslacht;
            if(inwoner.getSex()){
                geslacht = "man";
                
            }else{
                geslacht = "vrouw";
            }
            System.out.println("Patch: "+ patchNr + ", Naam: " + inwoner.getName() + ", Age: " + inwoner.getAge() + ", Geslacht: " + geslacht);
        }
    }
    
    /**
     * Print information on the population to a file.
     * @param w a Writer that will export the information to a file
     * @param time the current time-step (because we want this printed as well)
     */
    public void printCsvPop(PrintWriter w, int time){
        for(Individu inwoner:inwoners){
            String geslacht;
            if(inwoner.getSex()){
                geslacht = "man";
                
            }else{
                geslacht = "vrouw";
            }
            w.println(time+ "," + patchNr + ",\"" + inwoner.getName() + "\"," + inwoner.getAge() + ",\"" + geslacht + "\"");
        }
    }
    public void printCsvPop(PrintWriter w, int time, int run){
        for(Individu inwoner:inwoners){
            String geslacht;
            if(inwoner.getSex()){
                geslacht = "man";
                
            }else{
                geslacht = "vrouw";
            }
            w.println(run+","+time+ "," + patchNr + ",\"" + inwoner.getName() + "\"," + inwoner.getAge() + ",\"" + geslacht + "\"");
        }
    }
   
    public int[] populationCount(){
        int[] populatie = new int[2];
        int males = 0;
        int females = 0;
        for(Individu inwoner: inwoners){
            if(inwoner.getSex()){
                males +=1;
            }else{
                females += 1;
            }
        }
        populatie[1] = males;
        populatie[0] = females;
        return populatie;
    }
    
    private int[] reproductionCount(){
        int[] populatie = new int[2];
        int males = 0;
        int females = 0;
        for(Individu inwoner: inwoners){
            if(inwoner.getAge()>= reproductiveAge){
                if(inwoner.getSex()){
                    males +=1;
                }else{
                    females += 1;
                }
            }
        }
        populatie[1] = males;
        populatie[0] = females;
        return populatie;
    }
    
    /**
    * This function makes a random draw from the uniform [0,1] distribution and compares it with a chance of success.
    * @param p this is the chance of success
    */
    private boolean randomDraw(double p){
        boolean result;
        double faith = rg.nextDouble();
        result = faith < p;
        return result;
    }
    
    /**
     * Make a draw from a uniform [0,1] distribution and compare it with multiple chances of success.
     * the [0,1]-interval is divided into pieces proportional to the chances with which an event can happen.
     * The number of the event that's decided upon is returned
     * @param ps the chances of certain events happening (must sum to 1)
     * @return the number of the event that will happen according to the draw.
     */
    private int multiDraw(double[] ps){
        double faith = rg.nextDouble();
        int i = 0;
        while(i<ps.length && faith > arraySum(ps,i)){
            i++;
        }
        return i;
    }
    
    /**
    This method sums all elements in an array up until the specified position (including the element on that position).
    */
    private double arraySum(double[] array, int finalPos){
        double result = 0;
        for(int i = 0; i<=finalPos;i++){
            result += array[i];
        }
        return result;
    }
    
    private double demographicStoch(double p, double s){
        double result;
        if(p<1){
            int n = (int) Math.round(p*(1-p)/Math.pow(s, 2));
            Binomial bin = new Binomial(n,p);
            result = bin.random();
        }else{
            Normal norm = new Normal(p,s);
            result = norm.random();
        }
        return result;
    }
    
    /**
     * Change the sex ratio
     * @param ratio the new ratio
     */
    public void setSexRatio(double ratio){
        this.sexRatio = ratio;
    }
    /**
     * Change the maximal age
     * @param age the new maximal age
     */
    public void setMaxAge(int age){
        this.maxAge = age;
    }
    
    public void setJuvSurvival(double newSurv){
        this.overlevingsKans[1] = newSurv;
    }
    
    public int getPatchNr(){
        return this.patchNr;
    }
}
