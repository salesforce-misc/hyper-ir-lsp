from tableauhyperapi import HyperProcess, Telemetry, Connection
from tableauhyperapi.impl import hapi
import shutil
import glob
import os

hyper_path="/Users/avogelsgesang/repos/hyper-db/bazel-bin/hyper/tools/hyperd"
params={"dump_ir": "1", "ir_instruction_backtrace": "1"}

with HyperProcess(telemetry=Telemetry.SEND_USAGE_DATA_TO_TABLEAU, parameters=params, hyper_path=hyper_path) as hyper:
    pid = hapi.hyper_instance_get_pid(hyper._HyperProcess__cdata)
    with Connection(endpoint=hyper.endpoint) as connection:
        connection.execute_scalar_query("SELECT 1+5")
        codegen_folder = glob.glob(f"codegen_{pid}_*")[0]
        shutil.copyfile(f"{codegen_folder}/1_fcf.hir", "./fcf.hir")

        connection.execute_command("""
            CREATE TEMPORARY TABLE animals(name, legs) AS VALUES
                ('dog', 4),
                ('cat', 4),
                ('bird', 2),
                ('kangaroo', 2),
                ('centipede', 100)
            """)
        shutil.copyfile(f"{codegen_folder}/2_relation.hir", "./relation.hir")
        shutil.copyfile(f"{codegen_folder}/3_query.hir", "./insert.hir")

        max_legs = connection.execute_scalar_query(
            "SELECT MAX(legs) FROM animals")
        shutil.copyfile(f"{codegen_folder}/4_query.hir", "./query.hir")

        shutil.rmtree(f"{codegen_folder}")
        os.remove("hyperd.log")
