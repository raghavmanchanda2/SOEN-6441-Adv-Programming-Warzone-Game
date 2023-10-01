package model;

/**
 * Interface containing {@code start} method to contain/maintain the current game phase
 *
 * @author Ishaan Bajaj
 * @version 1.0.1
 */

public interface WarzoneController {
    /**
     * start method for WarzoneController
     *
     * @param p_CurrentPhase contains the game phase
     * @return returns the game phase
     * @throws Exception if an issue arises
     */
    GamePhaseEnum start(GamePhaseEnum p_CurrentPhase) throws Exception;
}