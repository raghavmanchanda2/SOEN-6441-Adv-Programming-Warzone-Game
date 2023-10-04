/**
 * The `CurrentGamePlay` class is responsible for executing the current game phase and implementing the WarzoneController interface.
 * It provides functionality related to managing the game phase, processing user commands, and interacting with the game model.
 *
 * <p>
 * This class defines a set of game-related commands and handles user input for those commands.
 * </p>
 *
 * @see WarzoneController
 * @version 1.0
 */
package controller;

import model.GamePhaseEnum;
import model.MapModel;
import model.WarzoneController;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

/**
 * The `CurrentGamePlay` class is responsible for executing the current game phase and implementing the WarzoneController interface.
 * It provides functionality related to managing the game phase, processing user commands, and interacting with the game model.
 *
 * <p>
 * This class defines a set of game-related commands and handles user input for those commands.
 * </p>
 *
 * @see WarzoneController
 * @author Rohit
 * @version 1.0
 */
public class CurrentGamePlay implements WarzoneController {

    private final List<String> Game_Commands = Arrays.asList("showmap", "loadmap", "gameplayer", "assigncountries", "savegame");
    private final Scanner s = new Scanner(System.in);
    MapModel d_MapModel;

    /**
     * Default Constructor for the `CurrentGamePlay` class.
     * Initializes the `d_MapModel` with an instance of `MapModel`.
     */
    public CurrentGamePlay() {
        d_MapModel = MapModel.getInstance();
    }

    /**
     * Starts the current game phase and handles game-related commands based on user input.
     *
     * @param p_CurrentPhase The current game phase to start.
     * @return The updated game phase after processing user commands.
     * @throws Exception If an error occurs during game phase execution.
     */
    @Override
    public GamePhaseEnum start(GamePhaseEnum p_CurrentPhase) throws Exception {
        // Implementation for starting and managing the current game phase
        // ...
        return null; // Update with the actual return value
    }
}
