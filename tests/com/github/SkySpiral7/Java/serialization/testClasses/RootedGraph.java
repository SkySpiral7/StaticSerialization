package com.github.SkySpiral7.Java.serialization.testClasses;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.github.SkySpiral7.Java.dataStructures.IdentityHashSet;
import com.github.SkySpiral7.Java.serialization.*;

public final class RootedGraph implements StaticSerializable
{
	private final Node root;

	public RootedGraph(final Node root)
	{
		this.root = root;
	}

	public static RootedGraph readFromStream(final ObjectStreamReader reader)
	{
		final ObjectReaderRegistry registry = reader.getObjectRegistry();

		final List<Node> allNodes = new ArrayList<>();
		final int nodeCount = reader.readObject(int.class);
		for (int nodeIndex = 0; nodeIndex < nodeCount; ++nodeIndex)
		{
			final String id = reader.readObject(String.class);
			final Node node = reader.readObject(Node.class);
			allNodes.add(node);
			registry.registerObject(id, node);
		}
		allNodes.forEach(node -> {
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
	public void writeToStream(final ObjectStreamWriter writer)
	{
		final ObjectWriterRegistry registry = writer.getObjectRegistry();

		final List<Node> allNodes = getAllNodes();
		writer.writeObject(allNodes.size());
		allNodes.forEach(node -> {
			writer.writeObject(registry.registerObject(node));
			writer.writeObject(node);
		});
		allNodes.forEach(node -> {
			writer.writeObject(node.links.size());
			node.links.forEach(linkedNode -> {
				writer.writeObject(registry.getId(linkedNode));
			});
		});
	}

	private List<Node> getAllNodes()
	{
		//I can't use IdentityHashSet alone because I must retain order
		final List<Node> result = new ArrayList<>();
		final Set<Node> visited = new IdentityHashSet<>();
		final Deque<Node> unexplored = new ArrayDeque<>();
		unexplored.add(root);
		while (!unexplored.isEmpty())
		{
			final Node cursor = unexplored.removeLast();
			if (visited.contains(cursor)) continue;
			result.add(cursor);
			visited.add(cursor);
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
		public final String data;
		public final List<Node> links = new ArrayList<>();

		public Node(final String data)
		{
			Objects.requireNonNull(data);
			this.data = data;
		}

		public static Node readFromStream(final ObjectStreamReader reader)
		{
			return new Node(reader.readObject(String.class));
		}

		@Override
		public void writeToStream(final ObjectStreamWriter writer)
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
