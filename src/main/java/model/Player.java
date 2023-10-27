package model;

import controller.OrderIssue;
import logger.ConsoleWriter;
import logger.Logger;
import model.orders.CreateOrder;
import model.orders.Order;
import model.player.PlayerStrategy;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Player class which holds the issue order and next order in the list of orders
 * @author Ishaan Bajaj
 * @version build 1
 */

public class Player {

    /**
     * integer to store the ID of player
     */
    private int d_Id;

    /**
     * integer to store the name of the player
     */
    private String d_Name;
    /**
     * A list of captured countries
     */
    private List<Country> d_CapturedCountries = new ArrayList<>();
    /**
     * A deque to manage the list of orders
     */
    private Deque<Order> d_Orders = new ArrayDeque<>();
    /**
     * An integer to store the number of reinforcement armies
     */
    private int d_ReinforcementArmies;

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    private final PlayerStrategy d_PlayerStrategy;

    /**
     * the constructor for player class
     */
    public Player(PlayerStrategy dPlayerStrategy) {
        d_PlayerStrategy = dPlayerStrategy;
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
    }


    /**
     * method to get armies issued
     *
     * @return issues armies
     */
    public int getD_ArmiesToIssue() {
        return d_ArmiesToIssue;
    }

    /**
     * method to set the armies issued
     * @param p_ArmiesToIssue armies to issue to player
     */
    public void setIssuedArmies(int p_ArmiesToIssue) {
        d_ArmiesToIssue = p_ArmiesToIssue;
    }

    /**
     * number of armies to issue
     */
    private int d_ArmiesToIssue = 0;


    /**
     * A function to get the player ID
     *
     * @return the ID of player
     */
    public int getId() {
        return d_Id;
    }

    /**
     * A function to set the player ID
     *
     * @param p_Id Player ID value
     */
    public void setId(int p_Id) {
        this.d_Id = p_Id;
    }

    /**
     *Get Player Name
     *
     * @return player name
     */
    public String getName() {
        return d_Name;
    }

    /**
     * Set Player Name
     *
     * @param p_Name Name of the player
     */
    public void setName(String p_Name) {
        this.d_Name = p_Name;
    }

    /**
     * A function to get the list of captured countries
     *
     * @return The list of captured countries
     */
    public List<Country> getCapturedCountries() {
        return d_CapturedCountries;
    }

    /**
     * A function to get the list of orders
     *
     * @return list of orders
     */
    public Deque<Order> getOrders() {
        return d_Orders;
    }


    /**     * A function to get the reinforcement armies for each player
     *
     * @return armies assigned to player of type int
     */
    public int getReinforcementArmies() {
        return d_ReinforcementArmies;
    }

    /**
     * A function to set the reinforcement armies for each player
     *
     * @param p_AssignedArmies armies assigned to player of type int
     */
    public void setReinforcementArmies(int p_AssignedArmies) {
        this.d_ReinforcementArmies = p_AssignedArmies;
    }


    public void addOrder(Order p_Order) {
        this.d_Orders.add(p_Order);
    }

    /**
     * A function to get the issue order from player and add to the order list
     */
    public void issueOrder() {
        Order l_Order = CreateOrder.createOrder(OrderIssue.PlayerCommands.split(" "), this);
        addOrder(l_Order);
    }

    /**
     * Calculate the number of the armies to be assigned in reinforcement phase.
     *
     * @param p_MapModel The game map object
     */
    public void calculateReinforcementArmies(MapModel p_MapModel) {
        if (!getCapturedCountries().isEmpty()) {
            int reinforcements = (int) Math.floor(getCapturedCountries().size() / 3f);
            setReinforcementArmies(reinforcements > 2 ? reinforcements : 3);
        } else {
            setReinforcementArmies(3);
        }
        d_ArmiesToIssue = getReinforcementArmies();
        d_logger.setLogMessage("The Player " + getName() + " is assigned with " + getReinforcementArmies() + " armies.");
    }

    public String readFromPlayer() {
        return this.d_PlayerStrategy.createCommand();
    }

    /**
     * method which returns the next order for execution
     *
     * @return order for executing for each player
     */
    public Order nextOrder() {
        return d_Orders.poll();
    }

    public String createACaptureList(List<Country> p_Capture) {
        String l_Result = "";
        for (Country l_Capture : p_Capture) {
            l_Result += l_Capture.getCountryId() + "-";
        }
        return l_Result.length() > 0 ? l_Result.substring(0, l_Result.length() - 1) : "";
    }

    /**
     * A function to check if the country exists in the list of player captured countries
     *
     * @param p_Country The country to be checked if present
     * @return true if country exists in the assigned country list else false
     */
    public boolean isCaptured(Country p_Country) {
        return d_CapturedCountries.contains(p_Country);
    }

    /**
     * A function to check if the army to deployed is valid
     *
     * @param p_ArmyCount The armies to be deployed to the country
     * @return true if the armies are valid and deducted from the assigned army pool else false
     */
    public boolean deployReinforcementArmiesFromPlayer(int p_ArmyCount) {
        if (p_ArmyCount > d_ReinforcementArmies || p_ArmyCount <= 0) {
            return false;
        }
        d_ReinforcementArmies -= p_ArmyCount;
        return true;
    }
    
    /**
     * A function to get the list of captured countries
     *
     * @param countryList
     */
    public void setCapturedCountries(List<Country> countryList) {
        this.d_CapturedCountries=countryList;
    }

}
