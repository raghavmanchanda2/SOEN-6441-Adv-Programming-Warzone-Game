package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.Player;
/**
 * The AssignReinforcements class is responsible for managing the assignment of reinforcement armies to players.
 * It calculates the number of reinforcement armies for each player and allows them to issue reinforcement orders.
 * @Author Ishaan
 * @version 1.0
 * @since 1.0
 */
public class AssignReinforcements {

    MapModel d_MapModel;

    Player d_Player;
    GamePhaseEnum d_GamePhaseEnum;

    private IssueOrder d_issueOrder;

    public AssignReinforcements() {
        d_MapModel = MapModel.getInstance();
        d_issueOrder = new IssueOrder();
    }
    /**
     * Manages the assignment of reinforcement armies to players.
     * Calculates the number of reinforcement armies for each player and allows them to issue reinforcement orders.
     * @throws Exception If an exception occurs during the reinforcement assignment.
     */
    public void assignReinforcements() throws Exception {

        calculateReinforcements();
        d_issueOrder.issue_order();
    }
    /**
     * Calculates the number of reinforcement armies for each player.
     * Calls the calculateReinforcementArmies method for each player.
     * @throws Exception If an exception occurs during the calculation of reinforcements.
     */
    public void calculateReinforcements() throws Exception {
        for (Player l_Player : d_MapModel.getPlayers().values()) {
            d_Player = l_Player;
            setReinforcementTroops();
        }
    }
    /**
     * Sets the reinforcement troops for the current player.
     * Calls the calculateReinforcementArmies method of the player to calculate reinforcements.
     * @throws Exception If an exception occurs during the calculation of reinforcements.
     */
    public void setReinforcementTroops() throws Exception {
        d_Player.calculateReinforcementArmies(d_MapModel);

    }
}
