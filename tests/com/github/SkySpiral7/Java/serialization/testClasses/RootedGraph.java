package com.github.SkySpiral7.Java.serialization.testClasses;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import com.github.SkySpiral7.Java.serialization.ObjectReader;
import com.github.SkySpiral7.Java.serialization.ObjectRegistry;
import com.github.SkySpiral7.Java.serialization.ObjectWriter;
import com.github.SkySpiral7.Java.serialization.StaticSerializable;

public final class RootedGraph implements StaticSerializable
{
	private final Node root;

	public RootedGraph(final Node root)
	{
		this.root = root;
	}

	public static RootedGraph readFromStream(final ObjectReader reader)
	{
		final ObjectRegistry registry = reader.getObjectRegistry();

		final List<Node> allNodes = new ArrayList<>();
		final int nodeCount = reader.readObject(int.class);
		for (int nodeIndex = 0; nodeIndex < nodeCount; ++nodeIndex)
		{
			final String id = reader.readObject(String.class);
			final Node node = reader.readObject(Node.class);
			allNodes.add(node);
			registry.registerObject(id, node);
		}
		allNodes.stream().forEach(node -> {
			final int linkSize = reader.readObject(int.class);
			for (int linkIndex = 0; linkIndex < linkSize; ++linkIndex)
			{
				final String id = reader.readObject(String.class);
				node.links.add(registry.getRegisteredObject(id));
			}
		});
		//0 is the root because it's the first returned by getAllNodes which is used by write
		return new RootedGraph(allNodes.get(0));
	}

	@Override
	public void writeToStream(final ObjectWriter writer)
	{
		final ObjectRegistry registry = writer.getObjectRegistry();

		final List<Node> allNodes = getAllNodes();
		writer.writeObject(allNodes.size());
		allNodes.stream().forEach(node -> {
			writer.writeObject(node.id);
			writer.writeObject(node);
		});
		allNodes.stream().forEach(node -> {
			writer.writeObject(node.links.size());
			node.links.stream().forEach(linkedNode -> {
				writer.writeObject(linkedNode.id);
			});
		});
	}

	private List<Node> getAllNodes()
	{
		//I can't use IdentityHashSet because I must retain order
		final List<Node> result = new ArrayList<>();
		final Set<String> visited = new HashSet<>();
		final Deque<Node> unexplored = new ArrayDeque<>();
		unexplored.add(root);
		while (!unexplored.isEmpty())
		{
			final Node cursor = unexplored.removeLast();
			if (visited.contains(cursor.id)) continue;
			result.add(cursor);
			visited.add(cursor.id);
			unexplored.addAll(cursor.links);
		}
		return result;
	}

	@Override
	public boolean equals(final Object obj)
	{
		if (!(obj instanceof RootedGraph)) return false;
		final RootedGraph other = (RootedGraph) obj;
		final List<Node> allOtherNodes = other.getAllNodes();
		final List<Node> allMyNodes = this.getAllNodes();
		if (allOtherNodes.size() != allMyNodes.size()) return false;
		for (int i = 0; i < allMyNodes.size(); ++i)
		{
			if (!allMyNodes.get(i).equals(allOtherNodes.get(i))) return false;
		}
		return true;
	}

	@Override
	public int hashCode()
	{
		int hash = 3;
		final List<Node> allNodes = this.getAllNodes();
		for (final Node node : allNodes)
		{
			hash ^= node.hashCode();
		}
		return hash;
	}

	@Override
	public String toString()
	{
		final List<Node> allNodes = this.getAllNodes();
		return allNodes.toString();
	}

	public static final class Node implements StaticSerializable
	{
		public final String id;
		public final String data;
		public final List<Node> links = new ArrayList<>();

		public Node(final String data)
		{
			Objects.requireNonNull(data);
			this.data = data;
			this.id = UUID.randomUUID().toString();
			//TODO: Nodes require a UID. this indicates a lacking design of ObjectRegistry
		}

		public static Node readFromStream(final ObjectReader reader)
		{
			return new Node(reader.readObject(String.class));
		}

		@Override
		public void writeToStream(final ObjectWriter writer)
		{
			writer.writeObject(data);
		}

		@Override
		public boolean equals(final Object obj)
		{
			if (!(obj instanceof Node)) return false;
			return this.data.equals(((Node) obj).data);
		}

		@Override
		public int hashCode()
		{
			return data.hashCode();
		}

		@Override
		public String toString()
		{
			return data;
		}
	}
}
