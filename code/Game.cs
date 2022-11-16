using Facepunch.Voxels;
using Sandbox;
using Sandbox.UI.Construct;
using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

//
// You don't need to put things in a namespace, but it doesn't hurt.
//
namespace Sandbox;

/// <summary>
/// This is your game class. This is an entity that is created serverside when
/// the game starts, and is replicated to the client. 
/// 
/// You can use this to create things like HUDs and declare which player class
/// to use for spawned players.
/// </summary>
public partial class MyGame : Sandbox.Game
{
	public MyGame()
	{
		Current = this;
	}

	/// <summary>
	/// A client has joined the server. Make them a pawn to play with
	/// </summary>
	public override void ClientJoined( Client client )
	{
		base.ClientJoined( client );

		// Create a pawn for this client to play with
		var pawn = new Pawn();
		client.Pawn = pawn;

		// Get all of the spawnpoints
		var spawnpoints = Entity.All.OfType<SpawnPoint>();

		// chose a random one
		var randomSpawnPoint = spawnpoints.OrderBy( x => Guid.NewGuid() ).FirstOrDefault();

		// if it exists, place the pawn there
		if ( randomSpawnPoint != null )
		{
			var tx = randomSpawnPoint.Transform;
			tx.Position = tx.Position + Vector3.Up * 50.0f; // raise it up
			pawn.Transform = tx;
		}

		VoxelWorld.Current.AddViewer( client );

		if ( VoxelWorld.Current.Initialized )
		{
			SendMapToClient( client );
		}
	}

	public override void PostLevelLoaded()
	{
		if ( !IsServer )
			return;

		Log.Info( "PostLevelLoaded!" );

		StartLoadMapTask();
	}

	private async void StartLoadMapTask()
	{
		var world = VoxelWorld.Create( 1337 );

		world.OnInitialized += OnMapInitialized;
		world.SetMaterials( "materials/procgen/voxel.vmat", "materials/procgen/voxel_translucent.vmat" );
		world.SetChunkRenderDistance( 4 );
		world.SetChunkUnloadDistance( 8 );
		world.SetChunkSize( 32, 32, 32 );
		world.SetSeaLevel( 48 );
		world.SetMaxSize( 32, 32, 128 );
		world.LoadBlockAtlas( "textures/blocks/blocks_color.atlas.json" );
		world.AddAllBlockTypes();
		world.AddBiome<EmptyBiome>();
		world.SetMinimumLoadedChunks( 8 );

		world.SetChunkGenerator<PerlinChunkGenerator>();

		var startChunkSize = 8;

		for ( var x = 0; x < startChunkSize; x++ )
		{
			for ( var y = 0; y < startChunkSize; y++ )
			{
				await GameTask.Delay( 100 );

				var chunk = world.GetOrCreateChunk(
					x * world.ChunkSize.x,
					y * world.ChunkSize.y,
					0
				);

				_ = chunk.Initialize();
			}
		}

		await GameTask.Delay( 500 );

		Log.Info( "Chunks generated! " + VoxelWorld.Current.Seed + " " + VoxelWorld.Current.SizeX );

		world.OnInitialized += onWorldInit;
		world.Initialize();
	}

	private void OnMapInitialized()
	{
		Log.Info( "Map has been initialized!" );
		var clients = Client.All.ToList();

		foreach ( var client in clients )
		{
			if ( client.IsValid() && client.Pawn.IsValid() )
			{
				SendMapToClient( client );
			}
		}
	}

	private void SendMapToClient( Client client )
	{
		Log.Info( "Sending map to client!" );
		VoxelWorld.Current.Send( client );
	}

	private void onWorldInit()
	{
		Log.Info( "World has been init" );
		var position = VoxelWorld.Current.Chunks.FirstOrDefault().Key;
		foreach (var player in Client.All)
		{
			player.Pawn.Position = new Vector3( position.x, position.y, position.z );
		}
	}
}
