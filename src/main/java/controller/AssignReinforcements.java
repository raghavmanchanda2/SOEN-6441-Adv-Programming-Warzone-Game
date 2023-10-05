package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.Player;

/**
 * class to allot reinforcements
 * @author IShaan Bajaj
 * @version build 1
 */
public class AssignReinforcements {

    MapModel d_MapModel;

    Player d_Player;
    GamePhaseEnum d_GamePhaseEnum;

    private OrderIssue d_issueOrder;

    /**
     * default constructor
     */
    public AssignReinforcements() {
        d_MapModel = MapModel.getInstance();
        d_issueOrder = new OrderIssue();
    }

    /**
     * method to calculate and assign reinforcement armies to players
     * @throws Exception if any error occurs
     */
    public void assignReinforcements() throws Exception {

        for (Player l_Player : d_MapModel.getPlayers().values()) {
            d_Player = l_Player;
            d_Player.calculateReinforcementArmies(d_MapModel);
        }
        d_issueOrder.issue_order();
    }

}
