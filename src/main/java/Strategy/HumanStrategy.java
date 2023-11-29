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
 * Class that defines basic methods for an HUMAN Strategy  inherited the Playerstrategy
 * 
 * @author Rohit
 * @version build 3
 */
public class HumanStrategy extends PlayerStrategy{

	private final String strategyName = "HUMAN";

	@Override
	public String getStrategyName() {
		return strategyName;
	}
	/**
	 *  The constructor for the HUMAN strategy 
	 * @param p_player the player who uses this strategy
	 * @param p_mapModel the map model the main play phase commands 
	 * @param p_mainPlayPhaseController the main play phase controller
	 * @param p_mainPlayPhaseBusinessCommands the main play phase business commands
	 */
	public HumanStrategy(Player p_player, MapModel p_mapModel, MainPlayPhaseController p_mainPlayPhaseController, MainPlayPhaseBusinessCommands p_mainPlayPhaseBusinessCommands) {
		super(p_player, p_mapModel, p_mainPlayPhaseController, p_mainPlayPhaseBusinessCommands);
	}

	@Override
	public ResponseWrapper createOrder() {

		return null;
	}

	@Override
	protected Country toAttack() {
		return null;
	}

	@Override
	protected Country toAttackFrom() {
		return null;
	}

	@Override
	protected Country toMoveFrom() {
		return null;
	}

	@Override
	protected Country toMoveTo() {
		return null;
	}

	@Override
	protected Country toDefend() {
		return null;
	}
	
	/**
	 * To create order for human strategy player
	 * @return the response wrapper 
	 */
	public ResponseWrapper humanOrderCreation() {
		try {
			return d_mainPlayPhaseController.getMainPlaySetUpCommands(d_player, d_mainPlayPhaseController.getMainPlaySetUpCommandsFromUser());
		} catch (GeneralException e) {
			e.printStackTrace();
		}
		
		return null;
	}

}


































