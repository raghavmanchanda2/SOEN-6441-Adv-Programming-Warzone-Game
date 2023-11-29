package Strategy;

import business.MainPlayPhaseBusinessCommands;
import business.Order.Order;
import controller.MainPlayPhaseController;
import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;

/**
 * Class that defines basic methods for an Cheater Strategy  inherited the Playerstrategy
 * 
 * @author Rohit
 * @version build 3
 */
public class CheaterStrategy extends PlayerStrategy{

	private final String strategyName = "CHEATER";

	@Override
	public String getStrategyName() {
		return strategyName;
	}

	/**
	 *  The constructor for the benevolent strategy 
	 * @param p_player the player who uses this strategy
	 * @param p_mapModel the map model the main play phase commands 
	 * @param p_mainPlayPhaseController the main play phase controller
	 * @param p_mainPlayPhaseBusinessCommands the main play phase business commands
	 */
	public CheaterStrategy(Player p_player, MapModel p_mapModel, MainPlayPhaseController p_mainPlayPhaseController, MainPlayPhaseBusinessCommands p_mainPlayPhaseBusinessCommands) {
		super(p_player, p_mapModel, p_mainPlayPhaseController, p_mainPlayPhaseBusinessCommands);
		// TODO Auto-generated constructor stub
	}

	@Override
	public ResponseWrapper createOrder() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Country toAttack() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Country toAttackFrom() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Country toMoveFrom() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Country toMoveTo() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Country toDefend() {
		// TODO Auto-generated method stub
		return null;
	}

}
