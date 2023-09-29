package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.WarzoneController;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

/**
 * class to execute current GamePhase and implement WarZoneController
 */

public class CurrentGamePlay implements WarzoneController {

    private final List<String> Game_Commands = Arrays.asList("showmap", "loadmap", "gameplayer", "assigncountries", "savegame");
    private final Scanner s = new Scanner(System.in);
    MapModel d_MapModel;

    /**
     * Default Constructor
     */
    public CurrentGamePlay(){
        d_MapModel = MapModel.getInstance();
    }
    @Override
    public GamePhaseEnum start(GamePhaseEnum p_CurrentPhase) throws Exception {
        return null;
    }
}
