package controller;

import logger.ConsoleWriter;
import logger.Logger;
import model.Country;
import model.MapModel;
import model.Player;
import model.orders.Order;

import java.util.*;



public class OrderIssue {

    private static Set<Player> skipPlayers = new HashSet<>();

    public static String PlayerCommands = null;

    MapModel d_MapModel;

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;
    private OrderExecute d_orderExecute;

    public OrderIssue() {
        d_MapModel = MapModel.getInstance();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
        d_orderExecute = new OrderExecute();
    }

    public void issue_order(){
        if (d_MapModel.getD_CurrentPlayer() == null) {
            d_MapModel.setD_CurrentPlayer(d_MapModel.getPlayers().entrySet().iterator().next().getValue());
        }
        //d_GamePhase = p_GamePhase;
        while (!(skipPlayers.size() == d_MapModel.getPlayers().size())) {
            for (Player l_Player : d_MapModel.getPlayers().values()) {
                if ((d_MapModel.getD_GameLoaded() && !(l_Player.getName().equalsIgnoreCase(d_MapModel.getD_CurrentPlayer().getName())))) {
                    continue;
                }
                if (!skipPlayers.isEmpty() && skipPlayers.contains(l_Player)) {
                    continue;
                }
                d_MapModel.setD_GameLoaded(false);
                d_MapModel.setD_CurrentPlayer(l_Player);
                boolean l_IssueCommand = false;
                while (!l_IssueCommand) {
                    gameProgress(l_Player);
                    PlayerCommands = l_Player.readFromPlayer();
                    if (Objects.isNull(PlayerCommands)) {
                        PlayerCommands = "";
                    }
                    if (!PlayerCommands.isEmpty()) {
                        l_IssueCommand = checkInput(PlayerCommands, l_Player);
                    }
                    if (PlayerCommands.equals("pass")) {
                        break;
                    }

                }
                if (!PlayerCommands.equals("pass")) {
                    d_logger.setLogMessage(l_Player.getName() + " has issued this order :- " + PlayerCommands);
                    l_Player.issueOrder();
                    d_logger.setLogMessage("The order has been added to the list of orders.");

                }
            }
            d_MapModel.setD_GameLoaded(false);
        }
        skipPlayers.clear();
        d_orderExecute.orderExecute();
        d_logger.setLogMessage("All phases executed as per Build 1 requirements. Ending game...");
        System.exit(0);

    }

    public boolean checkInput(String p_Commands, Player p_User){
        List<String> l_Commands = Arrays.asList("deploy");
        String[] l_CommandArr = p_Commands.split(" ");
        if (p_Commands.toLowerCase().contains("pass")) {
            AddToSetOfPlayers(p_User);
            return false;
        }
        if (!l_Commands.contains(l_CommandArr[0].toLowerCase())) {
            d_logger.setLogMessage("The command syntax is invalid." + p_Commands);
            return false;
        }
        if (!CheckLengthOfCommand(l_CommandArr[0], l_CommandArr.length)) {
            d_logger.setLogMessage("The command syntax is invalid." + p_Commands);
            return false;
        }
        switch (l_CommandArr[0].toLowerCase()) {
            case "deploy":
                try {
                    Integer.parseInt(l_CommandArr[2]);
                } catch (NumberFormatException l_Exception) {
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                if(Integer.parseInt(l_CommandArr[2]) < 0){
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                break;
            case "advance":
                try {
                    Integer.parseInt(l_CommandArr[3]);
                } catch (NumberFormatException l_Exception) {
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                break;

            default:
                break;

        }
        return true;
    }


    private static void AddToSetOfPlayers(Player p_Player) {
        skipPlayers.add(p_Player);
    }

    private static boolean CheckLengthOfCommand(String p_Command, int p_Length) {
        if (p_Command.contains("deploy")) {
            return p_Length == 3;
        } else if (p_Command.contains("bomb") || p_Command.contains("blockade") || p_Command.contains("negotiate") || p_Command.contains("savegame")) {
            return (p_Length == 2);
        } else if (p_Command.contains("airlift") || p_Command.contains("advance")) {
            return (p_Length == 4);
        }
        return false;
    }


    public void gameProgress(Player p_Player) {
        d_logger.setLogMessage("");
        d_logger.setLogMessage("****************************************");
        d_logger.setLogMessage("*********** ISSUE-ORDER PHASE **********");
        d_logger.setLogMessage("****************************************");
        d_logger.setLogMessage("");
        d_logger.setLogMessage("-> To deploy the armies : deploy countryID numarmies");
        d_logger.setLogMessage("-> To skip: pass");
        d_logger.setLogMessage("");
        d_logger.setLogMessage("***** Input any command to proceed *****");
        d_logger.setLogMessage("****(Getting input from the user...)****\n");
        String l_Table = "- %-15s- %-19s- %-22s %n";
        System.out.format("****************************************%n");
        System.out.format("  Player   !  Initial Assigned  !  Left Armies       %n");
        System.out.format("****************************************%n");
        System.out.format(l_Table, p_Player.getName(), p_Player.getReinforcementArmies(), p_Player.getD_ArmiesToIssue());
        System.out.format("****************************************%n\n");

        d_logger.setLogMessage("The countries assigned to the player are: ");
        System.out.format("****************************************%n");

        System.out.format(
                " Country name  ! Country Armies  ! Neighbors    %n");
        System.out.format(
                "****************************************%n");
        for (Country l_Country : p_Player.getCapturedCountries()) {
            String l_TableCountry = "- %-15s- %-15s- %-35s%n";
            String l_NeighborList = "";
            for (Country l_Neighbor : l_Country.getNeighbors()) {
                l_NeighborList += l_Neighbor.getCountryId() + "-";
            }
            System.out.format(l_TableCountry, l_Country.getCountryId(), l_Country.getArmies(), l_Country.createNeighboursList(l_Country.getNeighbors()));
        }
        System.out.format("****************************************\n");


        if (!p_Player.getOrders().isEmpty()) {
            d_logger.setLogMessage("The Orders issued by Player " + p_Player.getName() + " are:");
            for (Order l_Order : p_Player.getOrders()) {
                d_logger.setLogMessage(l_Order.getD_orderDetails().getD_Command());
            }
        }
    }
}
