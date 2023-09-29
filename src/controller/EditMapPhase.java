package controller;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import model.GamePhaseEnum;
import model.MapModel;
import model.WarzoneController;


/**
 *
 */
public class EditMapPhase implements WarzoneController{
    private Scanner s = new Scanner(System.in);
    
    MapModel d_MapModel;

    private final List<String> Game_Commands = Arrays.asList("editcontinent", "editcountry", "editneighbor",
            "showmap", "savemap", "editmap", "validatemap");

    /**
     * This is the default constructor
     */
    public EditMapPhase() {
        this.d_MapModel = MapModel.getInstance();
    }

    @Override
    public GamePhaseEnum start(GamePhaseEnum p_CurrentPhase) throws Exception {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean validateInput(List<String> p_list){
        if(p_list.size() > 0){
            String l_main = p_list.get(0);
            if (p_list.size() == 1){
                p_list.add("dummy");
            }
            return Game_Commands.contains(l_main.toLowerCase());
        }
        return false;
    }


}
