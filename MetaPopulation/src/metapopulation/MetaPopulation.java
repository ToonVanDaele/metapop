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
import java.util.Scanner;

/**
 *
 * @author Aranka
 */
public class MetaPopulation {

    private static ArrayList<Populatie> metaPopulatie;
    private static ArrayList<Double> patchAreas;
    private static ArrayList<Double[]> migration;
    private static ArrayList<ArrayList<Double>> juvSurvival;
//    private static ArrayList<ArrayList<Double>> adultSurvival;
//    private static ArrayList<ArrayList<Double>> reproduction;
    private static int time;
    private static int Tmax; //= 1000;
    private static int numPop;
    private static int nestsize; // = 4;
    private static double reprProb; // = 0.5;
    private static int reprAge; // = 2;
    private static boolean extinctionLoop;// = true;
    private static boolean oneLoop;// = false;
    private static String extinctionOutput;
    private static String populationOutput;
    private static String patchAreaInput;
    private static String migrationInput;
    private static String juvSurvInput;
//    private static String adSurvInput;
//    private static String reproductionInput;
    private static int[] initialAge;
    private static double[] survival;
    
    /**
     * Performs one time step.
     * In order: Reproduction --> Mortality --> Dispersal --> Ageing --> Carrying Capacity
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
     * Fills a matrix with stochastic juvenile survival values for each patch and each
     * timestep.
     *
     * @param inputFile the name of the .txt file with the wanted values
     * This file looks as follows: 
     * "tmax"  value
     * "numpop"  value
     * all juvenile survival values (one row per patch, one column per time step)
     * @return a nested ArrayList that represents a matrix of doubles
     * @throws IOException when the inputFile does not have the correct layout.
     */
    private static ArrayList<ArrayList<Double>> fillJuvenileSurvival(String inputFile) throws IOException {
        ArrayList<ArrayList<Double>> juvSurv = new ArrayList<>();
        File file3 = new File(inputFile);
        try {
            Scanner input = new Scanner(file3);
            for (int m = 1; m <= 2; m++) {
                String in1 = input.next();
                if (in1.equals("\"tmax\"")) {
                    Tmax = input.nextInt();
                } else {
                    if (in1.equals("\"numpop\"")) {
                        numPop = input.nextInt();
                    } else {
                        throw new IOException();
                    }
                }
            }
            for (int i = 0; i < numPop; i++) {
                juvSurv.add(i, new ArrayList<>());
                for (int j = 0; j < Tmax; j++) {
                    double d = input.nextDouble();
                    juvSurv.get(i).add(j, d);
                }
            }
        } catch (FileNotFoundException ex) {
            System.out.println("File not found");
        }
        return juvSurv;
    }
    /**
     * Same functionality as fillJuvenileSurvival
     * @param inputFile The name of the input file. 
     * This file only contains the survival values (one row per patch and one column per time step)
     * @return A matrix with the adult survival values (nested ArrayList)
     */
    private static ArrayList<ArrayList<Double>> fillAdultSurvival(String inputFile) {
        ArrayList<ArrayList<Double>> adultSurv = new ArrayList<>();
        File file4 = new File(inputFile);
        try {
            Scanner input = new Scanner(file4);
            for (int i = 0; i < numPop; i++) {
                adultSurv.add(i, new ArrayList<>());
                for (int j = 0; j < Tmax; j++) {
                    double d = input.nextDouble();
                    adultSurv.get(i).add(j, d);
                }
            }
        } catch (FileNotFoundException ex) {
            System.out.println("File not found");
        }
        return adultSurv;
    }

    private static ArrayList<ArrayList<Double>> fillReproduction(String inputFile) {
        ArrayList<ArrayList<Double>> repr = new ArrayList<>();
        File file5 = new File(inputFile);
        try {
            Scanner input = new Scanner(file5);
            for (int i = 0; i < numPop; i++) {
                repr.add(i, new ArrayList<>());
                for (int j = 0; j < Tmax; j++) {
                    double d = input.nextDouble();
                    repr.get(i).add(j, d);
                }
            }
        } catch (FileNotFoundException ex) {
            System.out.println("File not found");
        }
        return repr;
    }

    /**
     * Reads a file with patch areas and stores them.
     *
     * @param patchestxt
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
            System.out.println("File not found");
        }
    }

    /**
     * Reads a file with dispersion probabilities and stores them in a nested
     * ArrayList.
     * @param dispersal A txt file with the dispersal chances.
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
            System.out.println("File not found");
        }
    }
    
    /**
     * Performs a normal loop.
     * @param w a PrintWriter that will write out the information in the meta-population after the loop is finished
     */
    public static void loop(PrintWriter w) {
        while (time < Tmax) {
//            for(Populatie pop : metaPopulatie){
//                int popnum = pop.getPatchNr();
//                double v = reproduction.get(popnum).get(time-1);
//                double[] o = new double[3];
//                o[0] = 1;
//                o[1] = juvSurvival.get(popnum).get(time-1);
//                o[2] = adultSurvival.get(popnum).get(time-1);
//                pop.restore(v,o);
//            }
            for (Populatie pop : metaPopulatie) {
                int popnum = pop.getPatchNr();
                double o = juvSurvival.get(popnum).get(time - 1);
                pop.setJuvSurvival(o);
            }
            timeStep();
            /// Print out info for the timestep
            for (Populatie pop : metaPopulatie) {
                pop.printCsvPop(w, time);
            }
            System.out.println(time);
            time++;
        }
    }
    /**
     * Performs a loop meant to measure extinction
     * @param w1 a PrintWriter to write out the same info a loop(w)
     * @param w2 a PrintWriter to write out the extinction information
     * @param run the number of the run that needs to be printed.
     */
    public static void extinctionLoop(PrintWriter w1, PrintWriter w2, int run) {
        int totalPop = 1;
        while (time < Tmax) {
            totalPop = 0;
            for (Populatie pop : metaPopulatie) {
                int popnum = pop.getPatchNr();
                double o = juvSurvival.get(popnum).get(time - 1);
                pop.setJuvSurvival(o);
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
            if (totalPop == 0) {
                w2.println(run + ", " + time);
            }
            time++;
        }
    }
    /**
     * Fills all subpopulations in the meta-population with individuals based on the given parameters
     * @param reprAge reproductive age
     * @param survival array with the survival chances. 
     * survival[0]=newborn survival, survival[1]=juvenile survival, survival[2]=adult survival.
     * @param nestsize  mean nest size (will be Poisson distributed)
     * @param reprProb the reproductive probability
     * @param initialAge array with the initial age-distribution initialAge[i] = nr of individuals of age i.
     */
    public static void initializeMetaPop(int reprAge, double[] survival, int nestsize, double reprProb, int[] initialAge) {
        metaPopulatie = new ArrayList<>();
        for (int i = 0; i < patchAreas.size(); i++) {
            Populatie pop = new Populatie(20, i, reprAge, survival, patchAreas.get(i), reprAge, nestsize, reprProb, 0.5);
            System.out.println((Math.round(patchAreas.get(i) * 5)));
            pop.initialPopulation(initialAge);
            metaPopulatie.add(i, pop);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // Initialise default values if no args are given
        // This uses the standard dispersion matrix and patch areas of the 'snakes'-based example 
        //and a stochastic juvenile survival based on no correlation.
        if(args.length == 0){
            Tmax = 1000;
            extinctionLoop = true;
            oneLoop=false;
            nestsize = 3;
            reprAge = 2;
            reprProb = 0.5;
            survival = new double[3];
            survival[0] = 1;
            survival[1] = 0.5;
            survival[2] = 0.8;
            initialAge = new int[3];
            initialAge[2] = 5;
            patchAreaInput = "patches_default.txt";
            migrationInput = "markov_transition_default.txt";
            juvSurvInput = "juvSurvival_default.txt";
//            adSurvInput = "adultSurvival.txt";
//            reproductionInput = "reproduction.txt";
            populationOutput = "output.txt";
            extinctionOutput = "extinctionTime_noCorr.txt";
        }
        
        //Initialise everything that doesn't need user-input/default values
        createPatchAreas(patchAreaInput);
        numPop = patchAreas.size();
        createMigrationMat(migrationInput);
        try {
            juvSurvival = fillJuvenileSurvival(juvSurvInput);
//            adultSurvival = fillAdultSurvival(adSurvInput);
//            reproduction = fillReproduction(reproductionInput);
        } catch (IOException ex) {
            System.out.println("Wrong input format");
        }
        
        // Start simulation and write out information to the file output.txt
        try {
            if (extinctionLoop) {
                PrintWriter w1 = new PrintWriter(new BufferedWriter(new FileWriter(populationOutput)));
                PrintWriter w2 = new PrintWriter(new BufferedWriter(new FileWriter(extinctionOutput)));
                w1.println("run, timestep, patch, ID, age, sex");
                for (int i = 0; i < 500; i++) {
                    initializeMetaPop(reprAge, survival, nestsize, reprProb, initialAge);
                    for (Populatie pop : metaPopulatie) {
                        pop.printCsvPop(w1, 0); /// Initial population sizes
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
                initializeMetaPop(reprAge, survival, nestsize, reprProb, initialAge);
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
    }

}
