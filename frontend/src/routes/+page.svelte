<script lang="ts">
    import Time from "svelte-time";
    import {
        Button,
        Input,
        InputGroup,
        Table,
        Badge,
        Spinner,
        Card,
    } from "@sveltestrap/sveltestrap";
    import { onMount } from "svelte";

    type Result = {
        columns: string[];
        rows: any[][];
    };

    let querystr = $state("");
    let query: Promise<Result> | undefined = $state();
    let history: any[] = $state([]);
    let selectedId: number | null = $state(null);
    let selected = $derived(history.find((q) => q.id === selectedId));
    import { PUBLIC_API as api } from "$env/static/public";

    function runQuery() {
        query = fetch(api + "/query", { method: "POST", body: querystr }).then(
            (response) => {
                fetchHistory();
                return new Promise(function (resolve, reject) {
                    if (!response.ok)
                        response.text().then(err => err.trim() == "" ? reject(response.statusText) : reject(err)).catch(reject);
                    else return response.json().then(resolve).catch(reject);
                });
            },
        );
    }

    function fetchHistory() {
        fetch(api + "/history")
            .then((response) => response.json())
            .then((result) => (history = result));
    }

    onMount(fetchHistory);
</script>

{#snippet resultTable(result: Result)}
    <div class="table-responsive" style="height: unset;">
        <Table>
            <thead style="position: sticky; top: 0">
                <tr>
                    {#each result.columns as column}
                        <th>{column}</th>
                    {/each}
                </tr>
            </thead>
            <tbody>
                {#if result.rows.length > 0}
                    {#each result.rows as row}
                        <tr>
                            {#each row as column}
                                <td>{column}</td>
                            {/each}
                        </tr>
                    {/each}
                {/if}
            </tbody>
        </Table>
    </div>

    {#if result.rows.length == 0}
        <Card body>empty result set</Card>
    {/if}
{/snippet}

<div class="container">
    <div class="row" onclick={() => (selectedId = null)}>
        <div class="col">
            <InputGroup>
                <Input
                    bind:value={querystr}
                    type="text"
                    placeholder="SQL query"
                    onkeydown={(e) => {
                        if (e.key == "Enter") runQuery();
                    }}
                />
                <Button onclick={runQuery}>Send</Button>
            </InputGroup>
        </div>
    </div>

    <div class="row" style="flex: 1; overflow: hidden;">
        <div class="col" style="width: 50%;">
            <div class="table-responsive">
                <Table>
                    <thead style="position: sticky; top: 0;">
                        <tr>
                            <th>Timestamp</th>
                            <th>Query</th>
                            <th>Status</th>
                        </tr>
                    </thead>
                    <tbody>
                        {#each history as query (query.id)}
                            <tr
                                onclick={() => (selectedId = query.id)}
                                class:table-primary={selectedId == query.id}
                            >
                                <td>
                                    <Time
                                        timestamp={query.timestamp}
                                        format="YY-MM-DD HH:mm"
                                    />
                                </td>
                                <td>{query.query}</td>
                                <td>
                                    <Badge
                                        color={query.success
                                            ? "success"
                                            : "danger"}
                                        >{query.success ? "Ok" : "Error"}
                                    </Badge>
                                </td>
                            </tr>
                        {/each}
                    </tbody>
                </Table>
            </div>
        </div>
        <div class="col" style="display: flex; flex-direction:column; width: 50%;">
            <h3>Result</h3>
            {#if selectedId !== null}
                {#if selected.success}
                    {@render resultTable(JSON.parse(selected.result))}
                {:else}
                    <Card body color="danger">{selected.result}</Card>
                {/if}
            {:else if query === undefined}
                <Card body>Send a query</Card>
            {:else}
                {#await query}
                    <Spinner />
                {:then result}
                    {@render resultTable(result)}
                {:catch err}
                    <Card body color="danger">{err}</Card>
                {/await}
            {/if}
        </div>
    </div>
</div>

<style>
    .row {
        margin: 1em 0 1em 0;
    }

    .col {
        height: 100%;
    }

    .table-responsive {
        height: 100%;
    }

    .container {
        height: 100%;
        overflow: hidden;
        display: flex;
        flex-direction: column;
    }

    :global(.card) {
        flex: 0;
    }
</style>
