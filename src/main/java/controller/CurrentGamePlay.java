package controller;

import Constants.ProjectConfig;
import business.ExecuteMapsCommands;
import logger.ConsoleWriter;
import logger.Logger;
import model.GamePhaseEnum;
import model.MapModel;
import model.WarzoneController;
import persistence.OpenMap;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import static java.lang.System.exit;

/**
 * class to execute current GamePhase and implement WarZoneController
 * @author Ishaan Bajaj
 * @version build 1
 */

public class CurrentGamePlay {

    /**
     * List of game commands available at the StartUpPhase
     */
    private final List<String> Game_Commands = Arrays.asList("showmap",
            "loadmap",
            "gameplayer",
            "assigncountries");
    /**
     * to take input of user
     */
    private final Scanner s = new Scanner(System.in);
    /**
     * object of MapModel
     */
    MapModel d_MapModel;

    /**
     * To generate statements in the console
     */
    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    /**
     * data member of ExecuteMapsCommands class
     */
    private ExecuteMapsCommands d_executeMapsCommands;

    /**
     * data member of AssignReinforcements class
     */
    private AssignReinforcements d_AssignReinforcements;

    BufferedReader l_read;


    /**
     * Default Constructor of the class
     */
    public CurrentGamePlay(){
        d_MapModel = MapModel.getInstance();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
        d_executeMapsCommands = new ExecuteMapsCommands();
        d_AssignReinforcements = new AssignReinforcements();
    }

    /**
     * method to load already existing map file
     * @param p_Filename contains the user input map file name
     * @throws Exception if any error occurs while opening map
     */
    private void mapLoad(String p_Filename) throws Exception {
        boolean l_ShouldUseConquestAdapter = true;
        try {
            File l_File = new File(ProjectConfig.D_MAP_FILES_PATH+p_Filename);
            BufferedReader l_BufferedReader = new BufferedReader(new FileReader(l_File));
            while(l_BufferedReader.ready()) {
                String l_FirstLine = l_BufferedReader.readLine();
                if(! l_FirstLine.isEmpty()) {
                    if (l_FirstLine.contains(";")) {
                        l_ShouldUseConquestAdapter = false;
                    }
                    l_BufferedReader.close();
                }}}
        catch (IOException l_E) {
            // Do nothing.
        }
        OpenMap l_mapOpener =  new OpenMap();
        l_mapOpener.openMap(d_MapModel, p_Filename);
    }

    /**
     * method to start the startup phase and provide multiple options
     */
    public void startStartup() throws Exception{
        while (true) {
            d_logger.setLogMessage("");
            d_logger.setLogMessage("****************************************");
            d_logger.setLogMessage("************* STARTUP PHASE ************");
            d_logger.setLogMessage("****************************************");
            d_logger.setLogMessage("");
            d_logger.setLogMessage("-> To load the map : loadmap filename(.map)");
            d_logger.setLogMessage("-> To show the loaded map : showmap");
            d_logger.setLogMessage("-> To add a player : gameplayer -add playername");
            d_logger.setLogMessage("-> To remove a player : gameplayer -remove playername");
            d_logger.setLogMessage("-> To assign countries to players: assigncountries");
            d_logger.setLogMessage("-> To continue to Issue Orders Phase : continue");
            d_logger.setLogMessage("");
            d_logger.setLogMessage("***** Input any command to proceed *****");
            d_logger.setLogMessage("****(Getting input from the user...)****");


            String l_Input = s.nextLine();
            List<String> l_Inputs;
            if (l_Input.contains("-")) {
                l_Inputs = Arrays.stream(l_Input.split("-"))
                        .filter(s -> !s.isEmpty())
                        .map(String::trim)
                        .collect(Collectors.toList());
            } else {
                l_Inputs = Arrays.stream(l_Input.split(" ")).collect(Collectors.toList());
            }

            if (!checkInput(l_Inputs)) {
                if (l_Input.startsWith("continue")) {
                    l_Inputs.add(0, "continue");
                } else {
                    l_Inputs.clear();
                }
            }
            String l_userInput = l_Inputs.get(0);
            l_Inputs.remove(l_userInput);
            for (String l_restInput : l_Inputs) {
                String[] l_restInputsString = l_restInput.split(" ");

                if (l_userInput.toLowerCase().equals("loadmap")){
                    if (l_restInputsString.length == 1) {
                        mapLoad(l_restInputsString[0]);
                    }
                }
                else if (l_userInput.toLowerCase().equals("gameplayer")){
                    if (l_restInputsString.length > 0) {
                        if (l_restInputsString[0].equals("add")){
                            if (l_restInputsString.length == 2) {
                                d_MapModel.addPlayer(l_restInputsString[1]);
                            } else {

                            }
                        } else if (l_restInputsString[0].equals("remove")){
                            if (l_restInputsString.length == 2) {
                                d_MapModel.removePlayer(l_restInputsString[1]);
                            } else {

                            }
                        }
                    }
                } else if (l_userInput.toLowerCase().equals("assigncountries")) {
                    if (d_MapModel.getPlayers().size() > 1) {
                        d_MapModel.allot();
                    } else {
                        d_logger.setLogMessage("There should be minimum 2 players");
                    }
                }
                else if (l_userInput.toLowerCase().equals("showmap")) {
                    System.out.println("->Calling business file for showmap");
                    d_MapModel.showMap();
                }
                else if (l_userInput.toLowerCase().equals("continue")) {
                    d_AssignReinforcements.assignReinforcements();
                    exit(0);
                } else {
                    continue;
                }



            }
        }
    }

    /**
     * check input commands from the user
     * @param p_InputList List of strings bifurcated from the input
     * @return boolean if input command is correct or not.
     */

    public boolean checkInput(List<String> p_InputList) {
        if (!p_InputList.isEmpty()) {
            String l_command = p_InputList.get(0);
            if (p_InputList.size() == 1) {
                p_InputList.add("dummy");
            }
            return Game_Commands.contains(l_command.toLowerCase());
        }
        return false;
    }


}
