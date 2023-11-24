from tableauhyperapi import HyperProcess, Telemetry, Connection
from tableauhyperapi.impl import hapi
import shutil
import os

hyper_path="/home/avogelsgesang/Documents/hyper/main/bazel-bin/hyper/tools/hyperd"
params={"dump_ir": "1", "ir_instruction_backtrace": "1"}

with HyperProcess(telemetry=Telemetry.SEND_USAGE_DATA_TO_TABLEAU, parameters=params, hyper_path=hyper_path) as hyper:
    pid = hapi.hyper_instance_get_pid(hyper._HyperProcess__cdata)
    with Connection(endpoint=hyper.endpoint) as connection:
        connection.execute_scalar_query("SELECT 1+5")
        shutil.copyfile(f"codegen_{pid}/1_fcf.hir", "./fcf.hir")

        connection.execute_command("""
            CREATE TEMPORARY TABLE animals(name, legs) AS VALUES
                ('dog', 4),
                ('cat', 4),
                ('bird', 2),
                ('kangaroo', 2),
                ('centipede', 100)
            """)
        shutil.copyfile(f"codegen_{pid}/2_relation.hir", "./relation.hir")
        shutil.copyfile(f"codegen_{pid}/3_query.hir", "./insert.hir")

        max_legs = connection.execute_scalar_query(
            "SELECT MAX(legs) FROM animals")
        shutil.copyfile(f"codegen_{pid}/4_query.hir", "./query.hir")

        shutil.rmtree(f"codegen_{pid}")
        os.remove("hyperd.log")
