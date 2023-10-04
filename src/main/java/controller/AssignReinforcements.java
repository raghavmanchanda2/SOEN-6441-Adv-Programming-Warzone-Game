package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.Player;

public class AssignReinforcements {

    MapModel d_MapModel;

    Player d_Player;
    GamePhaseEnum d_GamePhaseEnum;

    private OrderIssue d_issueOrder;

    public AssignReinforcements() {
        d_MapModel = MapModel.getInstance();
        d_issueOrder = new OrderIssue();
    }

    public void assignReinforcements() throws Exception {

        for (Player l_Player : d_MapModel.getPlayers().values()) {
            d_Player = l_Player;
            d_Player.calculateReinforcementArmies(d_MapModel);
        }
        d_issueOrder.issue_order();
    }

    public void setReinforcementTroops() throws Exception {


    }
}
