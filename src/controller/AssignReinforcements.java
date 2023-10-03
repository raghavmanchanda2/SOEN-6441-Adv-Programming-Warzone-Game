package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.Player;

public class AssignReinforcements {

    MapModel d_MapModel;

    Player d_Player;
    GamePhaseEnum d_GamePhaseEnum;

    private IssueOrder d_issueOrder;

    public AssignReinforcements() {
        d_MapModel = MapModel.getInstance();
        d_issueOrder = new IssueOrder();
    }

    public void assignReinforcements() throws Exception {

        calculateReinforcements();
        d_issueOrder.issue_order();
    }

    public void calculateReinforcements() throws Exception {
        for (Player l_Player : d_MapModel.getPlayers().values()) {
            d_Player = l_Player;
            setReinforcementTroops();
        }
    }

    public void setReinforcementTroops() throws Exception {
        d_Player.calculateReinforcementArmies(d_MapModel);

    }
}
