package Strategy;

import business.MainPlayPhaseBusinessCommands;
import business.Order.Order;
import controller.MainPlayPhaseController;
import logger.GeneralException;
import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;

/**
 * Abstract Class that defines basic methods for Playerstrategy
 * 
 * @author Rohit
 * @version build 3
 */
public abstract class PlayerStrategy {

	Player d_player;
	MapModel d_mapModel;
	MainPlayPhaseController d_mainPlayPhaseController;
	MainPlayPhaseBusinessCommands d_mainPlayPhaseBusinessCommands;

	/**
	 *  The constructor for the player strategy 
	 * @param p_player the player who uses this strategy
	 * @param p_mapModel the map model the main play phase commands 
	 * @param p_mainPlayPhaseController the main play phase controller
	 * @param p_mainPlayPhaseBusinessCommands the main play phase business commands
	 */
	public PlayerStrategy(Player p_player, MapModel p_mapModel, MainPlayPhaseController p_mainPlayPhaseController, MainPlayPhaseBusinessCommands p_mainPlayPhaseBusinessCommands)
	{
		d_player = p_player;
		d_mapModel = p_mapModel;
		d_mainPlayPhaseController = p_mainPlayPhaseController;
		d_mainPlayPhaseBusinessCommands = p_mainPlayPhaseBusinessCommands;
	}

	/**
	 *  To get strategy name 
	 * @return Name of the strategy
	 */
	public abstract String getStrategyName();

	/**
	 * The basic create order method for player strategy
	 * @return the response wrapper
	 * @throws GeneralException the general exception
	 */
	public abstract ResponseWrapper createOrder() throws GeneralException;

	protected abstract Country toAttack();
	protected abstract Country toAttackFrom();

	protected abstract Country toMoveFrom();
	protected abstract Country toMoveTo();

	protected abstract Country toDefend();

}
