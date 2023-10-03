package controller;

import business.ExecuteMapsCommands;
import logger.ConsoleWriter;
import logger.Logger;
import model.GamePhaseEnum;
import model.MapModel;
import model.WarzoneController;
import persistence.OpenMap;

import javax.xml.bind.ValidationException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import static javafx.application.Platform.exit;

/**
 * class to execute current GamePhase and implement WarZoneController
 */

public class CurrentGamePlay {

    private final List<String> Game_Commands = Arrays.asList("showmap", "loadmap", "gameplayer", "assigncountries", "savegame");
    private final Scanner s = new Scanner(System.in);
    MapModel d_MapModel;

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    private ExecuteMapsCommands d_executeMapsCommands;

    private AssignReinforcements d_AssignReinforcements;


    /**
     * Default Constructor
     */
    public CurrentGamePlay(){
        d_MapModel = MapModel.getInstance();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
        d_executeMapsCommands = new ExecuteMapsCommands();
        d_AssignReinforcements = new AssignReinforcements();
    }

    private void loadMap(String p_Filename) throws Exception {
        boolean l_ShouldUseConquestAdapter = true;
        try {
            File l_File = new File("maps/"+p_Filename);
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

    public void startStartup() throws Exception {
        while (true) {
            d_logger.setLogMessage("\n************* StartUp Phase *************");
            d_logger.setLogMessage("-> GamePlay Commands Available: ");
            d_logger.setLogMessage("*****************************************");
            d_logger.setLogMessage("To load the map : loadmap filename");
            d_logger.setLogMessage("To show the loaded map : showmap");
            d_logger.setLogMessage("To add a player : gameplayer -add playername");
            d_logger.setLogMessage("To remove a player : gameplayer -remove playername");
            d_logger.setLogMessage("To assign countries to players: assigncountries");
            d_logger.setLogMessage("To exit the game : exit");
            d_logger.setLogMessage("*****************************************");
            d_logger.setLogMessage("-> Enter Command to proceed");
            d_logger.setLogMessage("(Getting input from the user....)");
            String l_Input = s.nextLine();
            List<String> l_InputList;
            if (l_Input.contains("-")) {
                l_InputList = Arrays.stream(l_Input.split("-"))
                        .filter(s -> !s.isEmpty())
                        .map(String::trim)
                        .collect(Collectors.toList());
            } else {
                l_InputList = Arrays.stream(l_Input.split(" ")).collect(Collectors.toList());
            }

            if (!checkInput(l_InputList)) {
                if (l_Input.startsWith("exit")) {
                    l_InputList.add(0, "exit");
                } else {
                    l_InputList.clear();
                    // if not available in command list forcing to call help
                    l_InputList.add("help");
                    l_InputList.add("dummy");
                }
            }
            String l_MainCommand = l_InputList.get(0);
            l_InputList.remove(l_MainCommand);
            for (String l_Command : l_InputList) {
                String[] l_Commands = l_Command.split(" ");
                switch (l_MainCommand.toLowerCase()) {
                    case "loadmap": {
                        if (l_Commands.length == 1) {
                            loadMap(l_Commands[0]);
                        }
                        break;
                    }
                    case "gameplayer": {
                        if (l_Commands.length > 0) {
                            switch (l_Commands[0]) {
                                case "add": {
                                    if (l_Commands.length == 2) {
                                        d_MapModel.addPlayer(l_Commands[1]);
                                    } else {

                                    }
                                    break;
                                }
                                case "remove": {
                                    if (l_Commands.length == 2) {
                                        d_MapModel.removePlayer(l_Commands[1]);
                                    } else {

                                    }
                                    break;
                                }
                            }
                        }
                        break;
                    }
                    //Handle assigncountries command from console

                    case "assigncountries": {
                        if (d_MapModel.getPlayers().size() > 1) {
                            d_MapModel.allot();
                        } else {
                            d_logger.setLogMessage("Game ended as the minimum players are not there.");
                            throw new ValidationException("Create atleast two players");
                        }
                        break;
                    }
                    //Handle showmap command from console
                    case "showmap": {
                        System.out.println("->Calling business file for showmap");
                        d_MapModel.showMap();
                        break;
                    }

                    case "exit": {
                        d_AssignReinforcements.assignReinforcements();
                        exit();
                        break;
                    }
                    //Print the commands for help
                    default: {


                    }
                }
            }
        }
    }

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
