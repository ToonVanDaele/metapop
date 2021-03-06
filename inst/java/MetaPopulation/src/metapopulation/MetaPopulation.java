/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metapopulation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Scanner;

/**
 *
 * @author Aranka Steyaert
 */
public class MetaPopulation {

    private static ArrayList<Populatie> metaPopulatie;
    private static ArrayList<Double> patchAreas;
    private static ArrayList<Double[]> migration;
    private static ArrayList<ArrayList<ArrayList<Double>>> stochSurvival;
    private static ArrayList<ArrayList<Double>> reproduction;
    private static int seed;
    private static int indivPha;
    private static int time;
    private static int burnIn;
    private static int tMax; 
    private static int numPop;
    private static int stages;
    private static int nestsize; 
    private static double reprProb; 
    private static int reprAge;
    private static int maxAge;
    private static int quasiExtinction;
    private static boolean extinctionLoop;
    private static boolean oneLoop;
    private static String extinctionOutput;
    private static String populationOutput;
    private static String patchAreaInput;
    private static String migrationInput;
    private static String stochInput;
    private static int[] initialAge;
    private static double[] survival;
    private static boolean[] stochasticity;
    private static int iterations;

    /**
     * Performs one time step. In order: Reproduction --> Mortality -->
     * Dispersal --> Ageing --> Carrying Capacity
     */
    public static void timeStep() {
        ArrayList<ArrayList<HashSet<Individu>>> dispersers = new ArrayList<>();
        /// Reproduction, mortality, getting dispersers
        for (int i = 0; i < metaPopulatie.size(); i++) {
            metaPopulatie.get(i).reproduction();
            metaPopulatie.get(i).mortality();
            dispersers.add(i, metaPopulatie.get(i).dispersal(migration.get(i)));
        }
        /// Adding dispersers to their new population
        for (int j = 0; j < metaPopulatie.size(); j++) {
            for (int k = 0; k < metaPopulatie.size(); k++) {
                metaPopulatie.get(j).addIndividuals(dispersers.get(k).get(j));
            }
        }
        /// Carrying capacity and ageing
        for (Populatie pop : metaPopulatie) {
            pop.ageing();
            pop.carryingCapacity();
        }
    }

    /**
     * Fills a matrix with stochastic survival values for each patch and each
     * time step, for all Stages.
     *
     * @param inputFile the name of the .txt file with the wanted values This
     * file looks as follows: 
     * "tmax" value 
     * "numpop" value 
     * "stages" value
     * "NameOfFirstStage" 0/1 
     * ... 
     * "NameOfLastStage" 0/1 
     * "Reproduction" 0/1 
     * all juvenile survival values (one row per patch, one column per time step)
     * @return a nested ArrayList that represents a matrix of doubles
     * @throws IOException when the inputFile does not have the correct layout.
     */
    private static void fillStochasticMatrices(String inputFile) throws IOException {
        stochSurvival = new ArrayList<>();
        File file3 = new File(inputFile);
        try {
            Scanner input = new Scanner(file3);
            for (int m = 0; m < 3; m++) {
                String in1 = input.next();
                if (in1.equals("\"tmax\"")) {
                    tMax = input.nextInt();
                } else {
                    if (in1.equals("\"numpop\"")) {
                        numPop = input.nextInt();
                    } else {
                        if (in1.equals("\"stages\"")) {
                            stages = input.nextInt();
                        } else {
                            throw new IOException();
                        }
                    }
                }
            }
            String check = "";
            stochasticity = new boolean[stages + 1];
            int j = 0;
            while (!check.equals("\"Reproduction\"") && j < stages + 1) {
                check = input.next();
                int yn = input.nextInt();
                stochasticity[j] = (yn == 1);
                j++;
            }
            for (int k = 0; k < stages; k++) {
                if (stochasticity[k]) {
                    ArrayList<ArrayList<Double>> stageSurv = new ArrayList<>();
                    for (int i = 0; i < numPop; i++) {
                        stageSurv.add(i, new ArrayList<>());
                        for (int l = 0; l < tMax; l++) {
                            double d = input.nextDouble();
                            stageSurv.get(i).add(l, d);
                        }
                    }
                    stochSurvival.add(k, stageSurv);
                } else {
                    stochSurvival.add(k, null);
                }
            }
            if (stochasticity[stages]) {
                reproduction = new ArrayList<>();
                for (int i = 0; i < numPop; i++) {
                    reproduction.add(i, new ArrayList<>());
                    for (int l = 0; l < tMax; l++) {
                        double d = input.nextDouble();
                        reproduction.get(i).add(l, d);
                    }
                }
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Stochasticity File not found");
        }
    }

    /**
     * Reads a file with patch areas and stores them.
     *
     * @param patchestxt The name of a .txt file with all the necessary information (patch number and patch size)
     */
    private static void createPatchAreas(String patchestxt) {
        patchAreas = new ArrayList<>();
        File file = new File(patchestxt);
        try {
            Scanner input = new Scanner(file);
            input.useDelimiter(",");
            input.nextLine();
            while (input.hasNextLine()) {
                int i = input.nextInt();
                double value = input.nextDouble();
                patchAreas.add(i - 1, value);
                input.nextLine();
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Patch Area File not found");
        }
    }

    /**
     * Reads a file with dispersion probabilities and stores them in a nested
     * ArrayList.
     *
     * @param dispersal The name of a .txt file with the dispersal chances.
     */
    private static void createMigrationMat(String dispersal) {
        migration = new ArrayList<>();
        for (int i = 0; i < numPop; i++) {
            migration.add(i, new Double[numPop]);
        }
        File file2 = new File(dispersal);
        try {
            Scanner input = new Scanner(file2);
            input.useDelimiter(",");
            input.nextLine();
            int j = 0;
            while (input.hasNextLine() && j < numPop) {
                for (int k = 0; k < numPop; k++) {
                    migration.get(k)[j] = input.nextDouble();
                }
                input.nextLine();
                j++;
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Migration File not found");
        }
    }

    /**
     * Performs a normal loop.
     *
     * @param w a PrintWriter that will write out the information in the
     * meta-population after the loop is finished
     */
    public static void loop(PrintWriter w) {
        boolean extinct = false;
        while (time < tMax && !extinct) {
            int totalPop = 0;
            for (Populatie pop : metaPopulatie) {
                int popnum = pop.getPatchNr();
                double v;
                if (stochasticity[stages]) {
                    v = reproduction.get(popnum).get(time - 1);
                } else {
                    v = reprProb;
                }
                double[] o = new double[stages + 1];
                o[0] = 1;
                for (int i = 0; i < stages; i++) {
                    if (stochasticity[i]) {
                        o[i + 1] = stochSurvival.get(i).get(popnum).get(time - 1);
                    } else {
                        o[i + 1] = survival[i + 1];
                    }
                }
                pop.restore(v, o);
            }
            timeStep();
            /// Print out info for the timestep
            for (Populatie pop : metaPopulatie) {
                pop.printCsvPop(w, time);
            }
            for (Populatie pop : metaPopulatie) {
                int[] popCount = pop.populationCount();
                for (int i = 0; i < popCount.length; i++) {
                    totalPop += popCount[i];
                }
            }
            if (totalPop <= quasiExtinction) {
                extinct = true;
            }
            time++;
        }
    }

    /**
     * Performs a loop meant to measure extinction
     *
     * @param w1 a PrintWriter to write out the same info a loop(w)
     * @param w2 a PrintWriter to write out the extinction information
     * @param run the number of the run that needs to be printed.
     */
    public static void extinctionLoop(PrintWriter w1, PrintWriter w2, int run) {
        int totalPop;
        boolean extinct = false;
        while (time < tMax && !extinct) {
            totalPop = 0;
            for (Populatie pop : metaPopulatie) {
                int popnum = pop.getPatchNr();
                double v;
                if (stochasticity[stages]) {
                    v = reproduction.get(popnum).get(time - 1);
                } else {
                    v = reprProb;
                }
                double[] o = new double[stages + 1];
                o[0] = 1;
                for (int i = 0; i < stages; i++) {
                    if (stochasticity[i]) {
                        o[i + 1] = stochSurvival.get(i).get(popnum).get(time - 1);
                    } else {
                        o[i + 1] = survival[i + 1];
                    }
                }
                pop.restore(v, o);
            }
            timeStep();
            for (Populatie pop : metaPopulatie) {
                pop.printCsvPop(w1, time, run);
            }
            for (Populatie pop : metaPopulatie) {
                int[] popCount = pop.populationCount();
                for (int i = 0; i < popCount.length; i++) {
                    totalPop += popCount[i];
                }
            }
            if (totalPop <= quasiExtinction) {
                w2.println(run + ", " + time);
                extinct = true;
            }
            time++;
        }
    }

    /**
     * Fills all subpopulations in the meta-population with individuals based on
     * the given parameters
     *
     * @param reprAge reproductive age
     * @param survival array with the survival chances. survival[0]=newborn
     * survival, survival[1]=juvenile survival, survival[2]=adult survival.
     * @param nestsize mean nest size (will be Poisson distributed)
     * @param reprProb the reproductive probability
     * @param initialAge array with the initial age-distribution initialAge[i] =
     * nr of individuals of age i.
     */
    public static void initializeMetaPop(int maxAge, int reprAge, double[] survival, int nestsize, double reprProb, int[] initialAge) {
        metaPopulatie = new ArrayList<>();
        for (int i = 0; i < patchAreas.size(); i++) {
            Populatie pop = new Populatie(maxAge,seed, indivPha, i, reprAge, survival, patchAreas.get(i), reprAge, nestsize, reprProb, 0.5);
            pop.initialPopulation(initialAge);
            metaPopulatie.add(i, pop);
        }
    }

    /**
     * @param args the command line arguments
     * Contains: seed(int) tMax(int) burnInTime(int) oneLoop(T/F) extinctionLoop(T/F) nestsize(int) reproductiveAge(int) maximutAge(int) maxIndivPerHa(int) reproductiveProb(double) survivalProb(double;double;...;double) initialAgeDistr(int;int;...;int) patchAreaInput(Strings) migrationInput(String) stochasticityInput(String) output1(String) output2(String)"
     */
    public static void main(String[] args) {
        // Initialise default values if no args are given
        // This uses the standard dispersion matrix and patch areas of the 'snakes'-based example 
        //and a stochastic juvenile survival based on no correlation.
        boolean initialised = false;
        if (args.length == 0) {
            seed = 420;
            tMax = 1000;
            iterations = 500;
            burnIn = 10;
            maxAge = 20;
            quasiExtinction = 10;
            extinctionLoop = true;
            oneLoop = false;
            nestsize = 3;
            reprAge = 2;
            reprProb = 0.5;
            indivPha = 5;
            survival = new double[3];
            survival[0] = 1;
            survival[1] = 0.5;
            survival[2] = 0.8;
            initialAge = new int[3];
            initialAge[2] = 5;
            patchAreaInput = "patches_default.txt";
            migrationInput = "markov_transition_default.txt";
            stochInput = "juvSurvival_default.txt";
            populationOutput = "Evolution.txt";
            extinctionOutput = "ExtinctionTimes.txt";
            initialised = true;
        }
        if (args.length == 1) {
            if (args[0].contains(".txt")) {
                File file = new File(args[0]);
                try {
                    Scanner read = new Scanner(file);
                    seed = read.nextInt();
                    tMax = read.nextInt();
                    iterations = read.nextInt();
                    burnIn = read.nextInt();
                    String oneLoopString = read.next();
                    oneLoop = (oneLoopString.equals("T")|| oneLoopString.equalsIgnoreCase("TRUE"));
                    String extinctionLoopString = read.next();
                    extinctionLoop = (extinctionLoopString.equals("T")|| extinctionLoopString.equalsIgnoreCase("TRUE"));
                    quasiExtinction = read.nextInt();
                    nestsize = read.nextInt();
                    reprAge = read.nextInt();
                    maxAge = read.nextInt();
                    indivPha = read.nextInt();
                    reprProb = read.nextDouble();
                    String surv = read.next();
                    String[] survs = surv.split(";");
                    stages = survs.length;
                    survival = new double[stages + 1];
                    survival[0] = 1;
                    for (int i = 1; i <= stages; i++) {
                        survival[i] = Double.parseDouble(survs[i - 1]);
                    }
                    String age = read.next();
                    String[] ages = age.split(";");
                    initialAge = new int[ages.length + 1];
                    initialAge[0] = 0;
                    for (int i = 1; i <= ages.length; i++) {
                        initialAge[i] = Integer.decode(ages[i - 1]);
                    }
                    patchAreaInput = read.next();
                    migrationInput = read.next();
                    stochInput = read.next();
                    if (read.hasNext()) {
                        populationOutput = read.next();
                    } else {
                        populationOutput = "Evolution.txt";
                    }
                    if (read.hasNext()) {
                        extinctionOutput = read.next();
                    } else {
                        extinctionOutput = "ExtinctionTimes.txt";
                    }
                    initialised = true;
                } catch (FileNotFoundException ex) {
                    System.out.println("The file " + args[0] + " was not found");
                } catch (NoSuchElementException ex2) {
                    System.out.println("Your file does not contain enough inputdata");
                }
            } else {
                System.out.println("Please enter the name of a .txt file with all the required information in the following order:");
                System.out.println("seed (int) tMax(int) nIterations(int) burnInTime(int) oneLoop(T/F) extinctionLoop(T/F) quasiExtinctionBound(int) nestsize(int) reproductiveAge(int) maximumAge(int) maxIndivPerHa(int) reproductiveProb(double) survivalProb(double;double;...;double) initialAgeDistr(int;int;...;int) patchAreaInput(Strings) migrationInput(String) stochasticityInput(String) output1(String) output2(String)");
                System.out.println("The last five Strings should be names of .txt files, the outputnames are optional");
            }
        }
        if(args.length >= 17 && args.length <= 19){
            seed = Integer.decode(args[0]);
            tMax = Integer.decode(args[1]);
            iterations = Integer.decode(args[2]);
            burnIn = Integer.decode(args[3]);
            oneLoop = (args[4].equals("T")||args[4].equalsIgnoreCase("TRUE"));
            extinctionLoop = (args[5].equals("T")||args[5].equalsIgnoreCase("TRUE"));
            quasiExtinction = Integer.decode(args[6]);
            nestsize = Integer.decode(args[7]);
            reprAge = Integer.decode(args[8]);
            maxAge = Integer.decode(args[9]);
            indivPha = Integer.decode(args[10]);
            reprProb = Double.parseDouble(args[11]);
            String[] survs = args[12].split(";");
            stages = survs.length;
            survival = new double[stages + 1];
            survival[0] = 1;
            for (int i = 1; i <= stages; i++) {
                survival[i] = Double.parseDouble(survs[i - 1]);
            }
            String[] ages = args[13].split(";");
            initialAge = new int[ages.length + 1];
            initialAge[0] = 0;
            for (int i = 1; i <= ages.length; i++) {
                initialAge[i] = Integer.decode(ages[i - 1]);
            }
            patchAreaInput = args[14];
            migrationInput = args[15];
            stochInput = args[16];
            if (args.length==18) {
                populationOutput = args[17];
                extinctionOutput = "ExtinctionTimes.txt";
            } else {
                if (args.length==19) {
                    populationOutput = args[17];
                    extinctionOutput = args[18];
                } else {
                    populationOutput = "Evolution.txt";
                    extinctionOutput = "ExtinctionTimes.txt";
                }
            }
            initialised = true;
        }
        
        if (initialised) {
            //Initialise everything that doesn't need user-input/default values
            createPatchAreas(patchAreaInput);
            numPop = patchAreas.size();
            createMigrationMat(migrationInput);
            try {
                fillStochasticMatrices(stochInput);
            } catch (IOException ex) {
                System.out.println("Wrong input format");
            }

            // Start simulation and write out information to the file output.txt
            try {
                if (extinctionLoop) {
                    PrintWriter w1 = new PrintWriter(new BufferedWriter(new FileWriter(populationOutput)));
                    PrintWriter w2 = new PrintWriter(new BufferedWriter(new FileWriter(extinctionOutput)));
                    w1.println("run, timestep, patch, ID, age, sex");
                    for (int i = 0; i < iterations; i++) {
                        boolean stable = false;
                        while(!stable){
                            initializeMetaPop(maxAge, reprAge, survival, nestsize, reprProb, initialAge);
                            for (int j  = 0; j<burnIn; j++){
                                timeStep();
                            }
                            int tot = 0;
                            for (Populatie pop : metaPopulatie) {
                                int[] popCount = pop.populationCount();
                                    for (int j = 0; j < popCount.length; j++) {
                                        tot += popCount[j];
                                }  
                            }
                            stable = (tot>quasiExtinction);
                        }
                        if(i==0){
                            patchAreas.stream().forEach((patch) -> {
                            System.out.println((Math.round(patch * 5)));
                            });
                        }
                        for (Populatie pop : metaPopulatie) {
                            pop.printCsvPop(w1, 0,i); /// Initial population sizes
                        }
                        time = 1;
                        extinctionLoop(w1, w2, i);
                    }
                    w1.close();
                    w2.close();
                }
                if (oneLoop) {
                    PrintWriter w1 = new PrintWriter(new BufferedWriter(new FileWriter(populationOutput)));
                    w1.println("timestep, patch, ID, age, sex"); ///Header
                    boolean stable = false;
                    while(!stable){
                        initializeMetaPop(maxAge, reprAge, survival, nestsize, reprProb, initialAge);
                        for (int j  = 0; j<burnIn; j++){
                            timeStep();
                        }
                        int tot = 0;
                        for (Populatie pop : metaPopulatie) {
                            int[] popCount = pop.populationCount();
                                for (int j = 0; j < popCount.length; j++) {
                                    tot += popCount[j];
                            }  
                        }
                        stable = (tot>quasiExtinction);
                    }
                    patchAreas.stream().forEach((patch) -> {
                        System.out.println((Math.round(patch * 5)));
                    });
                    for (Populatie pop : metaPopulatie) {
                        pop.printCsvPop(w1, 0); /// Initial population sizes
                    }
                    time = 1;
                    loop(w1);
                    w1.close();              
                }
            } catch (IOException ex) {
                System.out.println("File not found");
            }
        } else {
            System.out.println("Wrong input format");
            System.out.println("Allowed input:");
            System.out.println("1. No input --> Default example");
            System.out.println("2. One name of a .txt file containing the same info as in 3.");
            System.out.println("3. seed (int) tMax(int) burnInTime(int) oneLoop(T/F) extinctionLoop(T/F) nestsize(int) reproductiveAge(int) maximutAge(int) maxIndivPerHa(int) reproductiveProb(double) survivalProb(double;double;...;double) initialAgeDistr(int;int;...;int) patchAreaInput(Strings) migrationInput(String) stochasticityInput(String) output1(String) output2(String)");
        }
    }

}
