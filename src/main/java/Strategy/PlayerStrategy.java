package Strategy;

import business.MainPlayPhaseBusinessCommands;
import business.Order.Order;
import controller.MainPlayPhaseController;
import logger.GeneralException;
import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;

public abstract class PlayerStrategy {

	Player d_player;
	MapModel d_mapModel;
	MainPlayPhaseController d_mainPlayPhaseController;
	MainPlayPhaseBusinessCommands d_mainPlayPhaseBusinessCommands;

	public PlayerStrategy(Player p_player, MapModel p_mapModel, MainPlayPhaseController p_mainPlayPhaseController, MainPlayPhaseBusinessCommands p_mainPlayPhaseBusinessCommands)
	{
		d_player = p_player;
		d_mapModel = p_mapModel;
		d_mainPlayPhaseController = p_mainPlayPhaseController;
		d_mainPlayPhaseBusinessCommands = p_mainPlayPhaseBusinessCommands;
	}

	public abstract String getStrategyName();

	public abstract ResponseWrapper createOrder() throws GeneralException;

	protected abstract Country toAttack();
	protected abstract Country toAttackFrom();

	protected abstract Country toMoveFrom();
	protected abstract Country toMoveTo();

	protected abstract Country toDefend();

}
